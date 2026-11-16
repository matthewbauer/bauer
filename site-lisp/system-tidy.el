;;; system-tidy.el --- Periodic system tidying -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matthew Bauer

;; Author: Matthew Bauer <mjbauer95@gmail.com>
;; Version: 0.1
;; URL: https://github.com/matthewbauer/bauer
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, vc
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; system-tidy runs some maintenance tasks for you.  The commands are
;; intended to be run periodically, from `midnight-hook' (daily) and an
;; hourly timer:
;;
;; - `system-tidy-repos-maintenance' (daily), for each repo in
;;   `system-tidy-repos':
;;   - `jj util gc' for jj
;;   - `git maintenance run' for git
;; - `system-tidy-repos-prefetch' (hourly), for each repo in
;;   `system-tidy-repos':
;;   - prefetch from origin, so a later fetch has little to download,
;;     and update git's commit-graph
;;   - with :nix, keep the Nix dev shells of some branches built
;; - `system-tidy-nix-gc' (daily), Nix store garbage collection with
;;   `system-tidy-nix-gc-command', e.g. `nix-collect-garbage'
;;
;; Repo tasks are opt-in: `system-tidy-repos' is empty by default.
;; `system-tidy-nix-gc' runs `nix-collect-garbage' by default; set
;; `system-tidy-nix-gc-command' to nil to turn it off.
;;
;; Example setup with `use-package':
;;
;;   (use-package system-tidy
;;     :commands system-tidy-repos-prefetch
;;     :hook ((midnight . system-tidy-repos-maintenance)
;;            (midnight . system-tidy-nix-gc))
;;     :init (run-at-time 300 3600 #'system-tidy-repos-prefetch)
;;     :custom
;;     (system-tidy-repos '(("~/src/big-repo"
;;                           :nix (:branches ("main@origin")))
;;                          "~/src/other-repo")))
;;
;; `midnight-hook' only runs with `midnight-mode' enabled.  :hook
;; autoloads the hooked commands; the prefetch command, which isn't on a
;; hook, needs :commands.
;;
;; The commands share one queue, logged in *system-tidy*, so two steps
;; never run at once: maintenance and Nix gc wait for anything running,
;; and prefetch is skipped while anything is running.

;;; Code:

(require 'map)

(defgroup system-tidy nil
  "Periodic repo and Nix store cleanup."
  :group 'tools)

(defcustom system-tidy-repos nil
  "Repos to tidy.

:nix prebuilds Nix dev shells in `system-tidy-cache-directory',
updated by `system-tidy-repos-prefetch':
  :shell     `nix-shell' (the default) or `nix-develop'.
  :branches  Remote branches to prebuild shells for, as
             \"NAME@REMOTE\", or just \"NAME\" for NAME@origin. Each
             must be in REMOTE's configured fetch refspec, so that it
             is prefetched.

For example:

  ((\"~/src/big-repo\" :nix (:shell nix-develop
                             :branches (\"main\" \"release@upstream\")))
   \"~/src/other-repo\")

All listed repos are eligible for `system-tidy-repos-maintenance' and
`system-tidy-repos-prefetch' (prefetch and commit-graph for git repos).
Include your commonly-used repos, especially larger ones, for maximum
performance."
  :type '(repeat
          :tag "Repos"
          (choice
           :tag "Repo"
           (cons :tag "Directory with options"
                 (directory :tag "Directory")
                 (plist :tag "Options"
                        :options
                        ((:nix
                          (plist :tag "Nix dev shells"
                                 :options
                                 ((:shell (choice :tag "Shell"
                                                  (const nix-shell)
                                                  (const nix-develop)))
                                  (:branches
                                   (repeat :tag "Remote branches"
                                           (string :tag "NAME@REMOTE")))))))))
           (directory :tag "Directory"))))

(defcustom system-tidy-nix-gc-command '("nix-collect-garbage")
  "Command run to collect Nix store garbage, or nil to skip it."
  :type '(choice (const :tag "Skip" nil) (repeat string)))

(defcustom system-tidy-cache-directory
  (expand-file-name "emacs-system-tidy"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory for the source checkouts and GC roots of prebuilt dev shells."
  :type 'directory)

(defconst system-tidy--buffer "*system-tidy*"
  "Log buffer of the system-tidy commands.
A live process in it means steps are running.")

(defvar system-tidy--queue nil
  "Steps waiting to run.
Each is (DIRECTORY ENVIRONMENT . COMMAND), ENVIRONMENT being the
`process-environment' to run COMMAND with.")

(defvar system-tidy--report nil
  "Non-nil to say when the queue is done.  Set by interactive calls.")

(defun system-tidy--busy-p ()
  "Return non-nil if steps are running."
  (get-buffer-process system-tidy--buffer))

(defun system-tidy--report (was-busy)
  "Tell the user about the steps just queued.
WAS-BUSY means steps were already running.  Says when the queue is
done, too."
  (if (not (system-tidy--busy-p))
      (message "system-tidy: nothing to do")
    (setq system-tidy--report t)
    (message (if was-busy
                 "system-tidy: queued after the running steps; see %s"
               "system-tidy: running in the background; see %s")
             system-tidy--buffer)))

(defun system-tidy--run-if-not-busy (interactive queue-steps)
  "Call QUEUE-STEPS to queue steps, unless steps are already running.
INTERACTIVE means tell the user what happened, and when it's done."
  (if (system-tidy--busy-p)
      (when interactive
        (message "system-tidy: skipped, steps are running; see %s"
                 system-tidy--buffer))
    (funcall queue-steps)
    (when interactive
      (system-tidy--report nil))))

(defun system-tidy--run (dir &rest command)
  "Run COMMAND in DIR in the background, after the steps already queued.
DIR nil means the current `default-directory'.  COMMAND is flattened,
so it can include lists of arguments.  It runs with the current
`process-environment'.  Steps run one at a time, in order, logged to
`system-tidy--buffer'; failures are reported with `display-warning'."
  (let ((dir (file-name-as-directory (or dir default-directory))))
    (setq system-tidy--queue
          (append system-tidy--queue
                  (list `(,dir ,process-environment
                               ,@(flatten-tree command)))))
    (unless (system-tidy--busy-p)
      (system-tidy--next))))

(defun system-tidy--log (format-string &rest args)
  "Append FORMAT-STRING formatted with ARGS to `system-tidy--buffer'."
  (with-current-buffer (get-buffer-create system-tidy--buffer)
    (save-excursion
      (goto-char (point-max))
      (insert (apply #'format format-string args)))))

(defun system-tidy--next ()
  "Start the next queued step, if any."
  (when (and system-tidy--report (null system-tidy--queue))
    (setq system-tidy--report nil)
    (message "system-tidy: finished; see %s" system-tidy--buffer))
  (when-let* ((step (pop system-tidy--queue)))
    (pcase-let* ((`(,dir ,process-environment . ,command) step)
                 (default-directory dir)
                 (shown (combine-and-quote-strings command))
                 (start (current-time)))
      (system-tidy--log "\n== %s %s: %s\n"
                        (format-time-string "%F %T") dir shown)
      (condition-case err
          (make-process
           :name "system-tidy"
           :buffer system-tidy--buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (let ((status (process-exit-status proc)))
                 (system-tidy--log "== exit %d after %.1fs\n" status
                                   (float-time (time-since start)))
                 (unless (zerop status)
                   (display-warning
                    'system-tidy
                    (format "%s failed in %s (exit %d); see %s"
                            shown dir status system-tidy--buffer)
                    :error))
                 (system-tidy--next)))))
        ;; E.g. a missing program or directory.  Like a failed step,
        ;; report it and go on with the rest.
        (error
         (system-tidy--log "== could not start: %s\n"
                           (error-message-string err))
         (display-warning
          'system-tidy
          (format "%s could not start in %s: %s"
                  shown dir (error-message-string err)) :error)
         (system-tidy--next))))))

;;;###autoload
(defun system-tidy-repos-maintenance (&optional interactive)
  "Clean up the jj and git repos in `system-tidy-repos' in the background.
The repos are done one at a time, after any running steps.
INTERACTIVE (non-nil interactively) means report progress."
  (interactive (list t))
  (let ((was-busy (system-tidy--busy-p)))
    (pcase-dolist (`(,repo . ,_) (mapcar #'ensure-list system-tidy-repos))
      (let ((jj (file-directory-p (expand-file-name ".jj" repo)))
            (git (file-exists-p (expand-file-name ".git" repo))))
        (unless (or jj git)
          (display-warning 'system-tidy
                           (format "Not a jj or git repo: %s" repo)))
        (when jj
          (system-tidy--run repo
                            "jj" "--ignore-working-copy" "util" "gc"))
        (when git
          (system-tidy--run repo
                            "git" "maintenance" "run"
                            "--task=loose-objects" "--task=incremental-repack"
                            "--task=pack-refs"))))
    (when interactive
      (system-tidy--report was-busy))))

(defun system-tidy--dev-shell (repo remote-branch nix)
  "Queue steps to build the NIX dev shell of REMOTE-BRANCH in REPO.
REMOTE-BRANCH is \"NAME@REMOTE\" or just \"NAME\" (for origin).
This checks out the prefetched branch into a dedicated directory, since
evaluating the shell can need the whole source tree.  It uses its own
index rather than a git worktree, so it doesn't show up in REPO.  A
Nix GC root is then created for the latest shell closure."
  (pcase-let* ((`(,branch ,remote)
                (if (string-match "\\`\\(.+\\)@\\([^@]+\\)\\'" remote-branch)
                    (list (match-string 1 remote-branch)
                          (match-string 2 remote-branch))
                  (list remote-branch "origin")))
               (dir (expand-file-name
                     (concat (file-name-nondirectory (directory-file-name repo))
                             "/" (string-replace "/" "%" branch) "@" remote)
                     system-tidy-cache-directory))
               (source (file-name-as-directory (expand-file-name "source" dir)))
               (root (expand-file-name "gcroot" dir)))
    (make-directory source t)
    (let ((process-environment
           (cons (concat "GIT_INDEX_FILE=" (expand-file-name "index" dir))
                 process-environment)))
      (system-tidy--run repo
                        "git" (concat "--work-tree=" source)
                        "read-tree" "--reset" "-u"
                        (format "refs/prefetch/remotes/%s/%s" remote branch)))
    (pcase nix
      ('nix-develop
       (system-tidy--run source
                         "nix" "develop" "--profile" root "--command" "true")
       ;; Older generations would keep their shells alive too.
       (system-tidy--run source
                         "nix-env" "--profile" root
                         "--delete-generations" "old"))
      (_
       (system-tidy--run source
                         "nix-build" "shell.nix" "-A" "inputDerivation"
                         "-o" root)))))

;;;###autoload
(defun system-tidy-repos-prefetch (&optional interactive)
  "Prefetch and prepare the repos in `system-tidy-repos'.
Latest upstream Git commits are prefetched.  Repos with :nix also get their dev
shells rebuilt.  Runs in background.  Does nothing if any steps are
running.  INTERACTIVE (non-nil interactively) means report progress."
  (interactive (list t))
  (system-tidy--run-if-not-busy
   interactive
   (lambda ()
     (pcase-dolist (`(,repo . ,(map :nix))
                    (mapcar #'ensure-list system-tidy-repos))
       (when (file-exists-p (expand-file-name ".git" repo))
         ;; Fail rather than prompt for credentials on Emacs's terminal.
         (let ((process-environment
                (cons "GIT_TERMINAL_PROMPT=0" process-environment)))
           (system-tidy--run repo
                             "git" "maintenance" "run"
                             "--task=prefetch" "--task=commit-graph")))
       (pcase-let (((map :shell :branches) nix))
         (dolist (b branches)
           (system-tidy--dev-shell repo b (or shell 'nix-shell))))))))

;;;###autoload
(defun system-tidy-nix-gc (&optional interactive)
  "Collect Nix store garbage with `system-tidy-nix-gc-command' if set.
Runs in background, after any running steps.  INTERACTIVE (non-nil
interactively) means report progress."
  (interactive (list t))
  (let ((was-busy (system-tidy--busy-p)))
    (when system-tidy-nix-gc-command
      (system-tidy--run nil system-tidy-nix-gc-command))
    (when interactive
      (system-tidy--report was-busy))))

(provide 'system-tidy)
;;; system-tidy.el ends here
