;;; envrc.el --- Support for `direnv' that operates buffer-locally  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: processes, tools
;; Homepage: https://github.com/purcell/envrc
;; Package-Requires: ((emacs "26.1") (inheritenv "0.1"))
;; Package-Version: 0.12

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use direnv (https://direnv.net/) to set environment variables on a
;; per-buffer basis.  This means that when you work across multiple
;; projects which have `.envrc` files, all processes launched from the
;; buffers "in" those projects will be executed with the environment
;; variables specified in those files.  This allows different versions
;; of linters and other tools to be installed in each project if
;; desired.

;; Enable `envrc-global-mode' late in your startup files.  For
;; interaction with this functionality, see `envrc-mode-map', and the
;; commands `envrc-reload', `envrc-allow' and `envrc-deny'.

;; In particular, you can enable keybindings for the above commands by
;; binding your preferred prefix to `envrc-command-map' in
;; `envrc-mode-map', e.g.

;;    (with-eval-after-load 'envrc
;;      (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

;;; Code:

;; TODO: special handling for DIRENV_* vars? exclude them? use them to safely reload more aggressively?
;; TODO: special handling for remote files
;; TODO: handle nil default-directory (rarely happens, but is possible)
;; TODO: limit size of *direnv* buffer
;; TODO: special handling of compilation-environment?
;; TODO: handle use of "cd" and other changes of `default-directory' in a buffer over time?
;; TODO: describe env
;; TODO: click on mode lighter to get details
;; TODO: handle when direnv is not installed?
;; TODO: provide a way to disable in certain projects?
;; TODO: cleanup the cache?

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'ansi-color)
(require 'cl-lib)
(require 'diff-mode) ; for its faces
(require 'inheritenv)
(require 'comint)

;;; Custom vars and minor modes

(defgroup envrc nil
  "Apply per-buffer environment variables using the direnv tool."
  :group 'processes)

(defcustom envrc-debug nil
  "Whether or not to output debug messages while in operation.
Messages are written into the *envrc-debug* buffer."
  :type 'boolean)

(defcustom envrc-update-on-eshell-directory-change t
  "Whether envrc will update environment when changing directory in eshell."
  :type 'boolean)

(defcustom envrc-show-summary-in-minibuffer t
  "When non-nil, show a summary of the changes made by direnv in the minibuffer."
  :group 'envrc
  :type 'boolean)

(defcustom envrc-direnv-executable "direnv"
  "The direnv executable used by envrc."
  :type 'string)

(define-obsolete-variable-alias 'envrc--lighter 'envrc-lighter "2021-05-17")

(defcustom envrc-lighter '(:propertize (:eval (envrc--lighter))
                                       mouse-face mode-line-highlight
                                       local-map (keymap
                                                  (mode-line keymap
                                                             (down-mouse-1 . envrc-show-log))))
  "The mode line lighter for `envrc-mode'.
You can set this to nil to disable the lighter."
  :type 'sexp)
(put 'envrc-lighter 'risky-local-variable t)

(defcustom envrc-none-lighter nil
  "Lighter spec used by the default `envrc-lighter' when envrc is inactive."
  :type 'sexp)

(defcustom envrc-on-lighter '(" envrc[" (:propertize "on" face envrc-mode-line-on-face) "]")
  "Lighter spec used by the default `envrc-lighter' when envrc is on."
  :type 'sexp)

(defcustom envrc-updating-lighter '(" envrc[" (:propertize "updating" face envrc-mode-line-updating-face) "]")
  "Lighter spec used by the default `envrc-lighter' when envrc is updating."
  :type 'sexp)

(defcustom envrc-error-lighter '(" envrc[" (:propertize "error" face envrc-mode-line-error-face) "]")
  "Lighter spec used by the default `envrc-lighter' when envrc has errored."
  :type 'sexp)

(defcustom envrc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'envrc-allow)
    (define-key map (kbd "d") 'envrc-deny)
    (define-key map (kbd "r") 'envrc-reload)
    (define-key map (kbd "l") 'envrc-show-log)
    map)
  "Keymap for commands in `envrc-mode'.
See `envrc-mode-map' for how to assign a prefix binding to these."
  :type '(restricted-sexp :match-alternatives (keymapp)))
(fset 'envrc-command-map envrc-command-map)

(defcustom envrc-mode-map (make-sparse-keymap)
  "Keymap for `envrc-mode'.
To access `envrc-command-map' from this map, give it a prefix keybinding,
e.g. (define-key envrc-mode-map (kbd \"C-c e\") \\='envrc-command-map)"
  :type '(restricted-sexp :match-alternatives (keymapp)))

;;;###autoload
(define-minor-mode envrc-mode
  "A local minor mode in which env vars are set by direnv."
  :init-value nil
  :lighter envrc-lighter
  :keymap envrc-mode-map
  (if envrc-mode
      (progn
        (envrc--update)
        (when (and (derived-mode-p 'eshell-mode) envrc-update-on-eshell-directory-change)
          (add-hook 'eshell-directory-change-hook #'envrc--update nil t)))
    (envrc--clear (current-buffer))
    (remove-hook 'eshell-directory-change-hook #'envrc--update t)))

;;;###autoload
(define-globalized-minor-mode envrc-global-mode envrc-mode
  (lambda () (when (and (not (minibufferp)) (not (file-remote-p default-directory))
                        (executable-find envrc-direnv-executable))
               (envrc-mode 1))))

(defface envrc-mode-line-on-face '((t :inherit success))
  "Face used in mode line to indicate that direnv is in effect.")

(defface envrc-mode-line-error-face '((t :inherit error))
  "Face used in mode line to indicate that direnv failed.")

(defface envrc-mode-line-updating-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

(defface envrc-mode-line-none-face '((t :inherit warning))
  "Face used in mode line to indicate that direnv is not active.")

;;; Global state

(defvar envrc--cache (make-hash-table :test 'equal :size 10)
  "Known envrc directories and their direnv results.
The values are as produced by `envrc--export'.")

(defvar envrc--running-processes (make-hash-table :test 'equal :size 10)
  "Processes running by envrc.")

(defvar envrc--running-processes-callbacks (make-hash-table :test 'equal :size 10)
  "Callbacks for each process running by envrc.")

;;; Local state

(defvar-local envrc--status 'none
  "Symbol indicating state of the current buffer's direnv.
One of \\='(none on error).")

;;; Internals

(defun envrc--lighter ()
  "Return a colourised version of `envrc--status' for use in the mode line."
  (pcase envrc--status
    (`on envrc-on-lighter)
    (`updating envrc-updating-lighter)
    (`error envrc-error-lighter)
    (`none envrc-none-lighter)))

(defun envrc--env-dir-p (dir)
  "Return non-nil if DIR contains a config file for direnv."
  (or
   (file-exists-p (expand-file-name ".envrc" dir))
   (file-exists-p (expand-file-name ".env" dir))))

(defun envrc--find-env-dir ()
  "Return the envrc directory for the current buffer, if any.
This is based on a file scan.  In most cases we prefer to use the
cached list of known directories.

Regardless of buffer file name, we always use
`default-directory': the two should always match, unless the user
called `cd'"
  (let ((env-dir (locate-dominating-file default-directory #'envrc--env-dir-p)))
    (when env-dir
      ;; `locate-dominating-file' appears to sometimes return abbreviated paths, e.g. with ~
      (setq env-dir (expand-file-name env-dir)))
    env-dir))

(defun envrc--cache-key (env-dir process-env)
  "Get a hash key for the result of invoking direnv in ENV-DIR with PROCESS-ENV.
PROCESS-ENV should be the environment in which direnv was run,
since its output can vary according to its initial environment."
  (mapconcat 'identity (cons env-dir process-env) "\0"))

(defun envrc--update ()
  "Update the current buffer's environment if it is managed by direnv.
All envrc.el-managed buffers with this env will have their
environments updated."
  (let ((env-dir (envrc--find-env-dir))
        (buf (current-buffer)))
    (setq-local envrc--status 'updating)
    (if env-dir
        (let ((cache-key (envrc--cache-key env-dir (default-value 'process-environment))))
          (pcase (gethash cache-key envrc--cache 'missing)
            (`missing
             (envrc--export env-dir
                            (lambda (result)
                              (puthash cache-key result envrc--cache)
                              (envrc--apply buf result))))
            (cached (envrc--apply buf cached))))
      (envrc--apply buf 'none))))

(defvar envrc-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'envrc-reload)
    map))

;;;###autoload
(define-derived-mode envrc-log-mode special-mode "Envrc Log"
  "Major mode for Envrc log buffer.")

(defun envrc--log-buffer-name ()
  "Buffer name to use for envrc log buffers."
  (concat "*"
          (file-name-nondirectory
           (directory-file-name default-directory))
          "-envrc*"))

(defmacro envrc--at-end-of-special-buffer (name &rest body)
  "At the end of `special-mode' buffer NAME, execute BODY.
To avoid confusion, `envrc-mode' is explicitly disabled in the buffer."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create ,name)
     (unless (derived-mode-p 'envrc-log-mode)
       (envrc-log-mode))
     (when envrc-mode (envrc-mode -1))
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

(defun envrc--debug (msg &rest args)
  "A version of `message' which does nothing if `envrc-debug' is nil.
MSG and ARGS are as for that function."
  (when envrc-debug
    (envrc--at-end-of-special-buffer "*envrc-debug*"
      (insert (apply 'format msg args))
      (newline))))

(defun envrc--summarise-changes (items)
  "Create a summary string for ITEMS."
  (if items
      (cl-loop for (name . val) in items
               if (not (string-prefix-p "DIRENV_" name))
               collect (cons name
                             (if val
                                 (if (let ((process-environment (default-value 'process-environment)))
                                       (getenv name))
                                     '("~" diff-changed)
                                   '("+" diff-added))
                               '("-" diff-removed)))
               into entries
               finally return (cl-loop for (name prefix face) in (seq-sort-by 'car 'string< entries)
                                       collect (propertize (concat prefix name) 'face face) into strings
                                       finally return (string-join strings " ")))
    "no changes"))

(defun envrc--show-summary (result directory)
  "Summarise successful RESULT in the minibuffer.
DIRECTORY is the directory in which the environment changes."
  (message "direnv: %s %s"
           (envrc--summarise-changes result)
           (propertize (concat "(" (abbreviate-file-name (directory-file-name directory)) ")")
                       'face 'font-lock-comment-face)))

(defun envrc--export-new-process (env-dir callback)
  "Export the env vars for ENV-DIR using direnv."
  (let ((cache-key (envrc--cache-key env-dir (default-value 'process-environment)))
        result
        process)
    (message "Running direnv in %s..." env-dir)
    (puthash cache-key (cons callback (gethash cache-key envrc--running-processes-callbacks)) envrc--running-processes-callbacks)
    (puthash cache-key (envrc--make-process-with-global-env
                        `("direnv" "export" "json")
                        (lambda (exit-code stdout stderr)
                          (envrc--debug "Direnv exited with %s and stderr=%S, stdout=%S" exit-code stderr stdout)
                          (if (zerop exit-code)
                              (progn
                                (message "Direnv succeeded in %s" env-dir)
                                (if (length= stdout 0)
                                    (setq result 'none)
                                  (prog1
                                      (setq result (let ((json-key-type 'string)) (json-read-from-string stdout)))
                                    (when envrc-show-summary-in-minibuffer
                                      (envrc--show-summary result env-dir)))))
                            (message "Direnv failed in %s" env-dir)
                            (setq result 'error))
                          (envrc--at-end-of-special-buffer (envrc--log-buffer-name)
                            (insert "==== " (format-time-string "%Y-%m-%d %H:%M:%S") " ==== " env-dir " ====\n\n")
                            (let ((initial-pos (point)))
                              (insert (let (ansi-color-context) (ansi-color-apply stderr)))
                              (goto-char (point-max))
                              (add-face-text-property initial-pos (point) (if (zerop exit-code) 'success 'error)))
                            (insert "\n\n")
                            (unless (eq 0 exit-code) ;; zerop is not an option, as exit-code may sometimes be a symbol
                              (message "%s" stderr)))

                          ;; check again for callbacks in case they got added while this was running
                          (pcase (gethash cache-key envrc--running-processes-callbacks 'missing)
                            (`missing (error "envrc--export: no callbacks found"))
                            (callbacks
                             (remhash cache-key envrc--running-processes-callbacks)
                             (dolist (x callbacks) (funcall x result))))
                          (remhash cache-key envrc--running-processes)))
             envrc--running-processes)))

(defun envrc--export (env-dir callback)
  "Export the env vars for ENV-DIR using direnv."
  (unless (envrc--env-dir-p env-dir)
    (error "%s is not a directory with a .envrc" env-dir))
  (let ((cache-key (envrc--cache-key env-dir (default-value 'process-environment)))
        result)
    (pcase (gethash cache-key envrc--running-processes-callbacks 'missing)
      (`missing
       (envrc--export-new-process env-dir callback))
      (callbacks
       ;; Make sure process is still running.
       (let ((process (gethash cache-key envrc--running-process)))
         (if (and process (memq (process-status proc) '(open run stop)))
             (puthash cache-key (push callback callbacks) envrc--running-processes-callbacks)
           (progn
             (remhash cache-key envrc--running-process)
             (envrc--export-new-process env-dir callback))))))))

;; Forward declaration for the byte compiler
(defvar eshell-path-env)

(defun envrc--merged-environment (process-env pairs)
  "Make a `process-environment' value that merges PROCESS-ENV with PAIRS.
PAIRS is an alist obtained from direnv's output.
Values from PROCESS-ENV will be included, but their values will
be masked by Emacs' handling of `process-environment' if they
also appear in PAIRS."
  (append (mapcar (lambda (pair)
                    (if (cdr pair)
                        (format "%s=%s" (car pair) (cdr pair))
                      ;; Plain env name is the syntax for unsetting vars
                      (car pair)))
                  pairs)
          process-env))

(defun envrc--clear (buf)
  "Remove any effects of `envrc-mode' from BUF."
  (with-current-buffer buf
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)
    (kill-local-variable 'info-directory-list)
    (when (derived-mode-p 'eshell-mode)
      (if (fboundp 'eshell-set-path)
          (eshell-set-path (butlast exec-path))
        (kill-local-variable 'eshell-path-env)))))


(defun envrc--apply (buf result)
  "Update BUF with RESULT, which is a result of `envrc--export'."
  (with-current-buffer buf
    (setq-local envrc--status (if (listp result) 'on result))
    (if (memq result '(none error))
        (progn
          (envrc--clear buf)
          (envrc--debug "[%s] reset environment to default" buf))
      (envrc--debug "[%s] applied merged environment" buf)
      (setq-local process-environment (envrc--merged-environment (default-value 'process-environment) result))
      (let ((path (getenv "PATH"))) ;; Get PATH from the merged environment: direnv may not have changed it
        (setq-local exec-path (parse-colon-path path))
        (when (derived-mode-p 'eshell-mode)
          (if (fboundp 'eshell-set-path)
              (eshell-set-path path)
            (setq-local eshell-path-env path))))
      ;; Force info.el to parse INFOPATH again in case direnv modified it
      (when (getenv "INFOPATH") (setq-local Info-directory-list nil)))))

(defun envrc--update-env (env-dir)
  "Refresh the state of the direnv in ENV-DIR and apply in all relevant buffers."
  (envrc--debug "Invalidating cache for env %s" env-dir)
  (cl-loop for k being the hash-keys of envrc--cache
           if (string-prefix-p (concat env-dir "\0") k)
           do (remhash k envrc--cache))
  (envrc--debug "Refreshing all buffers in env  %s" env-dir)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (when (string= (envrc--find-env-dir) env-dir)
        (envrc--update)))))

(defun envrc--mode-buffers ()
  "Return a list of all live buffers in which `envrc-mode' is enabled."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (with-current-buffer b
                                 envrc-mode)))
              (buffer-list)))

(defmacro envrc--with-required-current-env (varname &rest body)
  "With VARNAME set to the current env dir path, execute BODY.
If there is no current env dir, abort with a user error."
  (declare (indent 1))
  (cl-assert (symbolp varname))
  `(let ((,varname (envrc--find-env-dir)))
     (unless ,varname
       (user-error "No enclosing .envrc"))
     ,@body))

(defun envrc--filter (proc string)
  "Process filter for envrc."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            ;; `save-excursion' doesn't use the right insertion-type for us.
            (pos (copy-marker (point) t))
            ;; `save-restriction' doesn't use the right insertion type either:
            ;; If we are inserting at the end of the accessible part of the
            ;; buffer, keep the inserted text visible.
	    (min (point-min-marker))
	    (max (copy-marker (point-max) t))
	    (envrc-filter-start (marker-position (process-mark proc))))
        (unwind-protect
            (progn
	      (widen)
	      (goto-char envrc-filter-start)
              (insert string)
              (comint-carriage-motion (process-mark proc) (point))
              (set-marker (process-mark proc) (point)))
	  (goto-char pos)
          (narrow-to-region min max)
	  (set-marker pos nil)
	  (set-marker min nil)
	  (set-marker max nil))))))

(defun envrc--make-process-with-global-env (command callback)
  "Starts a process with `make-process', but always use the global process environment.
In particular, we ensure the default variable `exec-path' and
`process-environment' are used.  This ensures an .envrc doesn't take
`envrc-direnv-executable' out of our path.
When the process is finished it will run the provided `callback' function with
the exit code, stdout and stderr of the process."
  (let* ((exec-path (default-value 'exec-path))
         (process-environment (default-value 'process-environment))
         (stderr-buffer (generate-new-buffer "*direnv stderr*"))
         (stdout-buffer (generate-new-buffer "*direnv*")))
    (with-current-buffer stderr-buffer (setq buffer-read-only t))
    (with-current-buffer stdout-buffer (setq buffer-read-only t))
    (make-process
     :name "direnv"
     :command command
     :buffer stdout-buffer
     :stderr stderr-buffer
     :connection-type 'pipe
     :noquery t
     :filter #'envrc--filter
     :sentinel (lambda (process event)
                 (unless (string= event "run\n")
                   (let* ((stdout (with-current-buffer (process-buffer process) (buffer-string)))
                          (stderr (with-current-buffer stderr-buffer (buffer-string))))
                     (funcall callback (process-exit-status process) stdout stderr)
                     (kill-buffer (process-buffer process))
                     (kill-buffer stderr-buffer)))))))

(defun envrc-reload ()
  "Reload the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (envrc--update-env env-dir)))

(defun envrc-allow ()
  "Run \"direnv allow\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir))
      (envrc--make-process-with-global-env
       `("direnv" "allow")
       (lambda (exit-code stdout stderr)
         (if (zerop exit-code)
             (with-current-buffer (get-buffer-create "*envrc-allow*")
               (erase-buffer)
               (insert stdout)
               (insert stderr)
               (display-buffer (current-buffer)))
           (user-error "Error running direnv allow")))))))

(defun envrc-deny ()
  "Run \"direnv deny\" in the current env."
  (interactive)
  (envrc--with-required-current-env env-dir
    (let* ((default-directory env-dir))
      (envrc--make-process-with-global-env
       `("direnv" "deny")
       (lambda (exit-code stdout stderr)
         (if (zerop exit-code)
             (with-current-buffer (get-buffer-create "*envrc-deny*")
               (erase-buffer)
               (insert stdout)
               (insert stderr)
               (display-buffer (current-buffer)))
           (user-error "Error running direnv deny")))))))

(defun envrc-reload-all ()
  "Reload direnvs for all buffers.
This can be useful if a .envrc has been deleted."
  (interactive)
  (envrc--debug "Invalidating cache for all envs")
  (clrhash envrc--cache)
  (dolist (buf (envrc--mode-buffers))
    (with-current-buffer buf
      (envrc--update))))

(defun envrc-show-log ()
  "Open envrc log buffer."
  (interactive)
  (if-let ((buffer (get-buffer "*envrc*")))
      (pop-to-buffer buffer)
    (message "Envrc log buffer does not exist")))


;;; Propagate local environment to commands that use temp buffers

(defun envrc-propagate-environment (orig &rest args)
  "Advice function to wrap a command ORIG and make it use our local env.
This can be used to force compliance where ORIG starts processes
in a temp buffer.  ARGS is as for ORIG."
  (if envrc-mode
      (inheritenv (apply orig args))
    (apply orig args)))

(advice-add 'shell-command-to-string :around #'envrc-propagate-environment)
(advice-add 'async-shell-command :around #'envrc-propagate-environment)
(advice-add 'org-babel-eval :around #'envrc-propagate-environment)


;;; Major mode for .envrc files

(defvar envrc-file-extra-keywords
  '("MANPATH_add" "PATH_add" "PATH_rm" "direnv_apply_dump" "direnv_layout_dir"
    "direnv_load" "direnv_version" "dotenv" "dotenv_if_exists"
    "env_vars_required" "expand_path" "fetchurl" "find_up" "has" "join_args"
    "layout" "load_prefix" "log_error" "log_status" "on_git_branch" "path_add"
    "path_rm" "rvm" "semver_search" "source_env" "source_env_if_exists"
    "source_up" "source_up_if_exists" "source_url" "strict_env" "unstrict_env"
    "use" "use_guix" "use_flake" "use_nix" "user_rel_path" "watch_dir" "watch_file")
  "Useful direnv keywords to be highlighted.")

(declare-function sh-set-shell "sh-script")

;;;###autoload
(define-derived-mode envrc-file-mode
  sh-mode "envrc"
  "Major mode for .envrc files as used by direnv.
\\{envrc-file-mode-map}"
  (sh-set-shell "bash")
  (font-lock-add-keywords
   nil `((,(regexp-opt envrc-file-extra-keywords 'symbols)
          (0 font-lock-keyword-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . envrc-file-mode))


(provide 'envrc)
;;; envrc.el ends here

;; LocalWords:  envrc direnv

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
