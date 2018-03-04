;;; installer -- Install Nix-based repo through Emacs.

;;; Commentary:

;; This will install or upgrade this Emacs distribution for you.
;; To use, load this file and type:

;; M-x install<RET>

;; within Emacs. Alternatively, you can see the README for other installation
;; methods.

;;; Code:

(require 'url-handlers)
(require 'comint)
(require 'subr-x)
(require 'timer)
(eval-when-compile (require 'cl))

(unless (locate-library "restart-emacs")
  (add-to-list 'load-path default-directory))
(require 'restart-emacs)

(defvar nix-profile (expand-file-name ".nix-profile"
                                      (getenv "HOME")))

(defgroup installer nil
  "Installer package"
  :group 'emacs)

(defcustom installer-repo-url "https://github.com/matthewbauer/bauer"
  "URL to clone with git."
  :group 'installer
  :type 'string)

(defcustom installer-nix-url "https://nixos.org/nix/install"
  "URL to download Nix installer from."
  :group 'installer
  :type 'string)

(defcustom installer-repo-dir
  (expand-file-name ".local/share/bauer" (getenv "HOME"))
  "Location of repo config."
  :group 'installer
  :type 'string)

(defcustom installer-auto-restart nil
  "Whether to auto restart Emacs on successful install."
  :group 'installer
  :type 'boolean)

(defcustom installer-auto-upgrade nil
  "Whether to auto upgrade Emacs using timer."
  :group 'installer
  :type 'boolean)

(defvar installer-out-path (expand-file-name "result" temporary-file-directory))

(defvar installer-running-process nil)

(defun is-exec (command)
  "Return true if `COMMAND' is an executable on the system search path."
  (file-executable-p
   (string-trim
    (shell-command-to-string
     (format "which %s" command)))))

(defun nix-emacs-path ()
  "Get relative path to Emacs."
  (pcase system-type
    ('darwin "Applications/Emacs.app/Contents/MacOS/Emacs")
    (- "bin/emacs")))

(defun restart-info (buffer)
  "Display info in BUFFER to restart Emacs."
  (lexical-let* ((installer-out-path (file-truename installer-out-path))
                 (emacs-binary
                  (file-truename
                   (expand-file-name (nix-emacs-path) installer-out-path)))
                 (old-emacs-binary (file-chase-links
                                    (expand-file-name (nix-emacs-path)
                                                      nix-profile))))
    (switch-to-buffer-other-window buffer)
    (unless (string= old-emacs-binary emacs-binary)
      (advice-add 'restart-emacs--get-emacs-binary
                  :override (lambda () emacs-binary))
      (shell-command (format "nix-env -i %s" installer-out-path) buffer)
      (when installer-auto-restart
        (restart-emacs)))
    (with-current-buffer buffer
      (if (string= old-emacs-binary emacs-binary)
          (progn
            (insert "\nEmacs is already up to date."))
        (progn (insert "\nEmacs updated!")
               (insert (format "\nYour new output path is %s."
                               installer-out-path))
               (insert "\nRun M-x restart-emacs to upgrade.")
               (insert "\n"))))))

(defun nix-install (&rest _)
  "Install Nix."
  (unless (is-exec "nix-build")
    (let ((nix-file (expand-file-name "nix.sh" temporary-file-directory)))
      (url-copy-file installer-nix-url nix-file t)
      (let ((current-mode (file-modes nix-file))
            (add-mode (logand ?\111 (default-file-modes))))
        (set-file-modes nix-file (logior current-mode add-mode)))
      (make-process :name "nix-installer"
                    :command (list nix-file)))))

(defun nix-update (&rest _)
  "Update Nix."
  (make-process :name "nix-channel"
                :command '("nix-channel" "--update")))

(defun repo-update (&rest _)
  "Update/install repo."
  (let* ((command
          (if (file-exists-p installer-repo-dir)
              `(,shell-file-name
                ,shell-command-switch
                ,(format "cd %s && git pull --no-rebase origin master"
                         installer-repo-dir))
            `("git" "clone" ,installer-repo-url ,installer-repo-dir))))
    (make-process :name "repo-update"
                  :command command)))

(defun repo-build (&rest _)
  "Build repo."
  (make-process :name "repo-install"
                :command `("nix-build"
                           "--out-link"
                           ,installer-out-path

                           ,installer-repo-dir)))

(defun run-sequentially (buffer fns)
  "Run each process in BUFFER generator, FNS, sequentially.
BUFFER is the buffer to show output in."
  (when fns
    (lexical-let ((proc (funcall (car fns) buffer))
                  (fns (cdr fns))
                  (buffer buffer))
      (when (processp proc)
        (setq installer-running-process proc)
        (with-current-buffer buffer
          (goto-char (point-max)))
        (set-process-buffer proc buffer)
        (set-process-sentinel proc (lambda (proc _)
                                     (when (and
                                            (eq 'exit (process-status proc))
                                            (= 0 (process-exit-status proc)))
                                       (run-sequentially buffer fns)))))
      (unless proc (run-sequentially buffer fns)))))

(defun install (&optional buffer)
  "Install Emacs.
BUFFER to show output in."
  (interactive)
  (when (process-live-p installer-running-process)
    (error "Can’t run two installers at once."))
  (when (not buffer) (setq buffer (get-buffer-create "*installer*")))
  (switch-to-buffer-other-window buffer)
  (unless (process-live-p installer-running-process)
    (with-current-buffer buffer
      (erase-buffer)
      (comint-mode)
      (local-set-key (kbd "q") 'quit-window)
      (run-sequentially buffer '(nix-install
                                 ;; nix-update
                                 repo-update
                                 repo-build
                                 restart-info)))))

(defun dev-restart (&optional buffer)
  "Development restart Emacs.
BUFFER to show output in."
  (interactive)

  (when (process-live-p installer-running-process)
    (error "Can’t run two installers at once."))

  ;; TODO move these into let bindings
  ;; (setq installer-auto-restart t)
  (when (file-exists-p (expand-file-name "default.nix" default-directory))
    (setq installer-repo-dir (expand-file-name default-directory)))

  (when (not buffer) (setq buffer (get-buffer-create "*dev*")))
  (switch-to-buffer-other-window buffer)
  (unless (process-live-p installer-running-process)
    (with-current-buffer buffer
      (erase-buffer)
      (comint-mode)
      (local-set-key (kbd "q") 'quit-window)
      (run-sequentially buffer '(repo-build
                                 restart-info)))))

(defun upgrade (&optional buffer)
  "Upgrade Emacs.
BUFFER to show output in."
  (interactive)

  (when (process-live-p installer-running-process)
    (error "Can’t run two installers at once."))

  (when (not buffer) (setq buffer (get-buffer-create "*upgrade*")))
  (when (and (not (process-live-p installer-running-process))
             (is-exec "nix-build"))
    (with-current-buffer buffer
      (erase-buffer)
      (comint-mode)
      (local-set-key (kbd "q") 'quit-window)
      (lexical-let ((installer-auto-restart nil))
        (run-sequentially buffer '(repo-update
                                   repo-build
                                   restart-info))))))

(when installer-auto-upgrade
  (run-with-timer 15 (* 24 60 60) 'upgrade))

(provide 'installer)
;;; installer.el ends here
