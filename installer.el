;;; installer -- Install Nix-based repo through Emacs.

;;; Commentary:

;;; Code:

(require 'url-handlers)
(require 'comint)
(require 'subr-x)
(require 'restart-emacs)
(eval-when-compile (require 'cl))

(defvar nix-profile (expand-file-name ".nix-profile" (getenv "HOME")))

(defgroup installer nil
  "Installer package"
  :group 'emacs)

(defcustom installer-repo-url "https://github.com/matthewbauer/nixpkgs-config"
  "URL to clone with git."
  :group 'installer)

(defcustom installer-nix-url "https://nixos.org/nix/install"
  "URL to download Nix installer from."
  :group 'installer)

(defcustom installer-repo-dir
  (expand-file-name "nixpkgs-config" temporary-file-directory)
  "Location of repo config."
  :group 'installer)

(defcustom installer-auto-restart nil
  "Whether to auto restart Emacs on successful install."
  :group 'installer
  :type 'boolean)

(defun is-exec (command)
  "Return true if `COMMAND' is an executable on the system search path."
  (file-executable-p
   (string-trim
    (shell-command-to-string
     (format "which %s" command)))))

(defun restart-info (buffer)
  "Display info in BUFFER to restart Emacs."
  (lexical-let* ((nix-output
                  (string-trim
                   (shell-command-to-string
                    (format "NIX_PATH=nixpkgs=%s nix-build %s"
                            (expand-file-name ".nix-defexpr/channels/nixpkgs"
                                              (getenv "HOME"))
                            installer-repo-dir))))
                 (emacs-binary (expand-file-name
                                "Applications/Emacs.app/Contents/MacOS/Emacs"
                                nix-output))
                 (old-emacs-binary (restart-emacs--get-emacs-binary)))
    (unless (string= old-emacs-binary emacs-binary)
      (advice-add 'restart-emacs--get-emacs-binary
                  :override (lambda () emacs-binary))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "\nEmacs repo buffer updated!")
        (insert "\nRun M-x restart-emacs to use.")
        (insert "\n"))
      (when installer-auto-restart
        (restart-emacs)))))

(defun nix-install (&rest _)
  "Install Nix."
  (unless (is-exec "nix-env")
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
  (let* ((git-command (format "git --work-tree=%s" installer-repo-dir))
         (command
          (if (file-exists-p installer-repo-dir)
              `(,shell-file-name
                ,shell-command-switch
                ,(format "%s fetch --all && %s reset --hard origin/master"
                         git-command git-command))
            `("git" "clone" ,installer-repo-url ,installer-repo-dir))))
    (make-process :name "repo-update"
                  :command command)))

(defun repo-install (&rest _)
  "Install repo."
  (make-process :name "repo-install"
                :command `("nix-env"
                           "-i"
                           "-f" ,installer-repo-dir)))

(defun run-sequentially (buffer fns)
  "Run each process in BUFFER generator, FNS, sequentially.
BUFFER is the buffer to show output in."
  (when fns
    (lexical-let ((proc (funcall (car fns) buffer))
                  (fns (cdr fns))
                  (buffer buffer))
      (when (processp proc)
        (set-process-buffer proc buffer)
        (set-process-sentinel proc (lambda (proc _)
                                     (when (and
                                            (eq 'exit (process-status proc))
                                            (= 0 (process-exit-status proc)))
                                       (run-sequentially buffer fns)))))
      (unless proc (run-sequentially buffer fns)))))

(defun install (&optional buffer)
  "Upgrade/install Emacs.
BUFFER to show output in."
  (interactive)
  (when (not buffer)
    (setq buffer (get-buffer-create "*installer*")))
  (switch-to-buffer-other-window buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (comint-mode)
    (local-set-key (kbd "q") 'quit-window)
    (run-sequentially buffer '(nix-install
                               nix-update
                               repo-update
                               repo-install
                               restart-info))))

(provide 'installer)
;;; installer.el ends here
