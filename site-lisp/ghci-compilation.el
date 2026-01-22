;;; ghci-compilation.el --- a GHCI comint buffer that supports compile-goto-error -*- lexical-binding: t; -*-

;; Author: Matthew Bauer <matthew@mercury.com>

;;; Commentary:

(require 'comint)
(require 'files)
(require 'shell)
(require 'compile)
(require 'haskell-mode)
(require 'haskell-completions)
(require 'transient)
(require 'comint-hyperlink)

(defvar-local ghci-compilation-running nil)
(defvar-local ghci-compilation-has-loaded-prompt nil)

(defun ghci-compilation-buffer-name (package)
  "Generate a name for ghci compilation buffer."
  (concat "*"
          "ghci-compilation"
          (when package (concat "-" package))
          "*"
          "<"
          (project-root (project-current))
          ">"))

(defun ghci-compilation-get-buffer (package)
  "Try to find the ghci-compilation buffer."
  (if ghci-compilation-running
      (current-buffer)
    (get-buffer (ghci-compilation-buffer-name package))))

(defvar ghci-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'ghci-compilation-reload)
    (define-key map "\C-c\C-g" 'ghci-compilation-reload)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)
    (define-key map "\M-n" 'ghci-compilation-next)
    (define-key map "\M-p" 'ghci-compilation-previous)
    (define-key map "g" 'ghci-compilation-maybe-refresh)
    (define-key map "\C-m" 'ghci-compilation-send-input)
    (define-key map "\t" 'ghci-compilation-maybe-completion-at-point)
    map))

(define-derived-mode ghci-compilation-mode comint-mode "Ghci Compilation mode"
  "Major mode for Ghci Compilation Mode.")

(defun ghci-compilation (&optional buffer flake-ref package ghci-repl-command)
  "Start ghci."
  (interactive
   (list
    nil
    "."
    nil
    nil))
  (unless buffer (setq buffer (get-buffer-create (ghci-compilation-buffer-name package))))
  (unless flake-ref (setq flake-ref "."))
  (unless ghci-repl-command
    (setq ghci-repl-command (if (string-suffix-p "mercury-web-backend/" (project-root (project-current)))
                                '("mwb-ghci")
                              '("cabal" "repl"))))
  (let* ((proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer))
         (buffer-env (append (list "PAGER=" (format "INSIDE_EMACS=%s,ghci-compilation" emacs-version)) (copy-sequence process-environment)))
         (proj (project-current)))
    (unless proc-alive
      (setq-local ghci-compilation-running t)

      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))

        (let ((inhibit-read-only t))
          (insert "\n======================\nStarting MWB GHCI...\n======================\n\n"))

        (ghci-compilation-mode)

        ;; performance
        (show-paren-mode -1)
        (setq-local bidi-inhibit-bpa nil)

        (setq-local comint-terminfo-terminal "xterm-256color")
        (setq-local comint-prompt-regexp "^ghci> ")
        (setq-local comint-prompt-read-only t)
        (setq-local comint-use-prompt-regexp t)
        (setq-local comint-process-echoes nil)
        (setq-local process-environment buffer-env)
        (setq-local default-directory (project-root proj))
        (setq-local ghci-compilation-has-loaded-prompt nil)
        (setq-local comint-input-ring-file-name (expand-file-name "ghci-compilation-history" user-emacs-directory))

        (apply 'make-comint-in-buffer "ghci-compilation" buffer "nix" nil "develop" flake-ref "-c" ghci-repl-command)

        (comint-read-input-ring 'silent)

        (setq-local compilation-error-regexp-alist `((,(concat
                                                        "^ *\\([^\n\r\t>]*\s*> \\)?" ;; if using multi-package stack project, remove the package name that is prepended
                                                        "\\(?1:[^\t\r\n]+?\\):"
                                                        "\\(?:"
                                                        "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
                                                        "\\|"
                                                        "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
                                                        "\\)"
                                                        ":\\(?6:\n?[ \t]+[Ww]arning:\\)?")
                                                      1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

                                                     ;; multiple declarations
                                                     ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
                                                      1 2 4 0) ;; info locus
                                                     ))
        (setq-local jit-lock-defer-time nil)
        (setq-local revert-buffer-function 'ghci-compilation-revert-buffer)
        (compilation-shell-minor-mode 1)

        (add-hook 'kill-buffer-hook 'comint-write-input-ring 'local)

        (add-hook 'comint-dynamic-complete-functions 'ghci-compilation-complete-at-point nil 'local)
        (setq-local comint-dynamic-complete-functions (delete 'comint-filename-completion comint-dynamic-complete-functions))
        (add-hook 'comint-output-filter-functions 'ghci-compilation-process-output nil 'local)
        (add-hook 'comint-output-filter-functions 'comint-hyperlink-process-output nil 'local)
        (add-hook 'comint-preoutput-filter-functions 'ghci-compilation-preoutput-filter nil 'local)

        (let ((pkgdir (concat (project-root proj) "/local-packages/" package)))
          (if (file-directory-p pkgdir)
              (add-to-list 'compilation-search-path pkgdir)
            (add-to-list 'compilation-search-path (project-root proj))))))
    (pop-to-buffer buffer)))

(defun ghci-compilation--send-string (text &optional buffer package)
  "Send `TEXT' to the inferior Haskell REPL process"
  (if-let* ((buffer (ghci-compilation-get-buffer package))
	    (process (get-buffer-process buffer)))
      (if (with-current-buffer buffer ghci-compilation-has-loaded-prompt)
          (comint-send-string process (concat text "\n"))
        (erfror "Haskell REPL process is starting up"))
    (warn "No Haskell REPL process is running")))

(defun ghci-compilation--is-running (&optional package)
  "Is ghci-compilation running?"
  (if-let* ((buffer (ghci-compilation-get-buffer package)))
      (with-current-buffer buffer ghci-compilation-has-loaded-prompt)))

(defun ghci-compilation-revert-buffer (ignore-auto noconfirm)
  "Revert buffer"
  (if (comint-check-proc (current-buffer))
      (ghci-compilation-reload)
    (ghci-compilation (current-buffer))))

(defun ghci-compilation-preoutput-filter (text)
  "Make output read only."
  text
  ;; (propertize text 'read-only t)
  )

(defun ghci-compilation-process-output (&optional _)
  "Process output to check if ghci has loaded."
  (unless ghci-compilation-has-loaded-prompt
    (let ((start-marker (if (and (markerp comint-last-output-start)
			         (eq (marker-buffer comint-last-output-start)
				     (current-buffer))
			         (marker-position comint-last-output-start))
			    comint-last-output-start
			  (point-min-marker)))
	  (end-marker (process-mark (get-buffer-process (current-buffer)))))
      (when (string-match "ghci> [\n]*\\'" (buffer-substring-no-properties start-marker end-marker))
        (setq ghci-compilation-has-loaded-prompt t)))))

(defun ghci-compilation-save-files ()
  "Save files in project."
  (interactive)
  (let ((proj (project-current)))
    (save-some-buffers t (and proj (memq (current-buffer) (project-buffers proj))))))

(defun ghci-compilation-reload (&optional package)
  "Reload ghci."
  (interactive)

  (if ghci-compilation-has-loaded-prompt
      (let ((buffer (ghci-compilation-get-buffer package)))
        (when compilation-locs (compilation-forget-errors))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (ghci-compilation-save-files)
        (ghci-compilation--send-string ":r"))
    (ghci-compilation)))

(defun ghci-get-module-name ()
  "Get the module name from a filename. Cabal is stupid & can’t tell when these are the same."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^module \\([a-zA-Z.]+\\)[( ]" nil t)
    (match-string-no-properties 1)))

(defun ghci-add-file ()
  "Add file to ghci."
  (interactive)
  (let ((module-name (ghci-get-module-name))
        (fn (file-relative-name (buffer-file-name) (project-root (project-current)))))
    (ghci-compilation-save-files)
    (ghci-compilation--send-string (format ":module +%s" module-name))))

(defun ghci-compilation-test-file ()
  "Add file to ghci."
  (interactive)
  (let ((fn (file-relative-name (buffer-file-name) (project-root (project-current))))
        (module-name (ghci-compilation-get-module-name))
        (is-web (save-excursion
                  (goto-char (point-min))
                  (re-search-forward ":: *SpecWeb" nil t))))
    (ghci-compilation-save-files)
    (ghci-compilation--send-string (concat
                                    (format ":add %s\n" module-name)
                                    (if is-web (format ":import-spec-web\nimport %s\nhspecWithEnv spec\n" module-name) "hspec spec\n")))))

(defun ghci-compilation-reload-nix (&optional package)
  "Reload nix too."
  (interactive)
  (let* ((buffer (ghci-compilation-get-buffer package))
         (proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (comint-write-input-ring)
      (let ((inhibit-read-only t))
        (and proc (delete-process proc)))
      (ghci-compilation buffer))))

(defun ghci-compilation-load-all (&optional package)
  "Load all modules."
  (interactive)

  (let ((buffer (ghci-compilation-get-buffer package)))
    (unless buffer (error "run ghci-compilation first"))

    (ghci-compilation--send-string ":add Application TestMain\n")))

(defun ghci-compilation-switch-to-buffer (&optional package)
  "Load all modules."
  (interactive)

  (let ((buffer (ghci-compilation-get-buffer package)))
    (unless buffer (error "run ghci-compilation first"))

    (switch-to-buffer buffer)))

(defun ghci-compilation-next (arg)
  "Go to next error, or if at prompt go to next input."
  (interactive "*p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (compilation-next-error arg)))

(defun ghci-compilation-previous (arg)
  "Go to previous error, or if at prompt go to previous input."
  (interactive "*p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (compilation-previous-error arg)))

(defun ghci-compilation-maybe-refresh (arg)
  "Refresh the shell, or if at prompt insert character."
  (interactive "*p")
  (if (comint-after-pmark-p)
      (self-insert-command arg)
    (ghci-compilation-reload)))

(defun ghci-compilation-send-input (&optional no-newline artificial)
  "Send input"
  (interactive nil comint-mode)
  (let ((inhibit-read-only t))
    (when compilation-locs (clrhash compilation-locs))
    (ghci-compilation-save-files)
    (comint-send-input)))

(defun ghci-compilation-maybe-completion-at-point (arg)
  "Indent, or if at prompt complete at point."
  (interactive "*p")
  (if (comint-after-pmark-p)
      (completion-at-point)
    (indent-for-tab-command arg)))

(defun ghci-compilation--completions (buf prefix)
  "Complete at point using ghci’s :complete command."
  (let* ((cmd (format ":complete repl 100 \"%s\"" prefix))
         (lines (let ((inhibit-quit nil)) (comint-redirect-results-list-from-process (get-buffer-process buf) cmd "^\\(.*\\)$" 1)))
         (header (split-string-and-unquote (nth 0 lines) " "))
         (num-results (string-to-number (nth 0 header)))
         (prefix (nth 2 header))
         (completions nil))
    (dotimes (i num-results)
      (push (concat prefix (nth 0 (split-string-and-unquote (nth (+ 1 i) lines) " "))) completions))
    completions))

(defun ghci-compilation-complete-at-point ()
  "Complete at point using ghci’s :complete command."
  (when-let* ((start (comint-line-beginning-position))
              (end (point)))
    (when (> end start)
      (list start end (ghci-compilation--completions (current-buffer) (buffer-substring-no-properties start end)) nil))))

(defun ghci-compilation-haskell-completion-at-point (&optional package)
  (let ((prefix-data (haskell-completions-grab-prefix))
        (is-blank-import (save-excursion
                           (goto-char (line-beginning-position))
                           (re-search-forward
                            (rx "import"
                                (? (1+ space) "qualified")
                                (1+ space)
                                )
                            (line-end-position)
                            t)
                           )))
    (cond
     (prefix-data
      (cl-destructuring-bind (beg end pfx typ) prefix-data
        (when (and (not (eql typ 'haskell-completions-general-prefix))
                   (or haskell-completions-complete-operators
                       (not (save-excursion
                              (goto-char (1- end))
                              (haskell-mode--looking-at-varsym)))))
          (unless (cl-member
                   typ
                   '(haskell-completions-pragma-name-prefix
                     haskell-completions-ghc-option-prefix
                     haskell-completions-language-extension-prefix))
            (let* ((is-import (eql typ 'haskell-completions-module-name-prefix))
                   (candidates
                    (when (ghci-compilation--is-running)
                      (ghci-compilation--completions (ghci-compilation-get-buffer package) (if is-import
                                                                                               (concat "import " pfx)
                                                                                             pfx))
                      )))
              (when is-import
                (setq candidates (mapcar (lambda (candidate) (string-remove-prefix "import " candidate)) candidates)))
              (list beg end candidates))))))
     (is-blank-import ;; special support for just "import " completion (no module name)
      (let ((candidates
             (when (ghci-compilation--is-running)
               (ghci-compilation--completions (ghci-compilation-get-buffer package) "import ")
               )))
        (setq candidates (mapcar (lambda (candidate) (string-remove-prefix "import " candidate)) candidates))
        (list (line-end-position) (line-end-position) candidates))
      ))))

;;;###autoload (autoload 'ghci-compilation-transient-menu "ghci-compilation" nil t)
(transient-define-prefix ghci-compilation-transient-menu ()
  "Emacs helpers for working at Mercury"
  ["ghci-compilation: Emacs helpers for working a Mercury"
   ["Actions"
    ("a" "Start/Open Session" ghci-compilation)
    ("r" "Reload" ghci-compilation-reload)
    ("R" "Reload Nix" ghci-compilation-reload-nix)
    ("f" "Next error follow minor mode" next-error-follow-minor-mode)
    ("A" "Add file" ghci-compilation-add-file)
    ("L" "Load all" ghci-compilation-load-all)
    ("t" "Test file" ghci-compilation-test-file)
    ]
   ])

(defun ghci-compilation-bind-keys ()
  "Bind keys"
  (interactive)
  (bind-keys
   ("C-x p ." . ghci-compilation)
   ("C-x C-1" . ghci-compilation-transient-menu))
  (bind-keys
   :map haskell-mode-map
   ("C-c C-l" . ghci-compilation-add-file)
   ("C-c C-z" . ghci-compilation-switch-to-buffer)))

(defun ghci-compilation-hooks ()
  "Add some hooks."
  (add-hook 'haskell-mode-hook (lambda ()
                                 (add-to-list 'completion-at-point-functions 'ghci-compilation-haskell-completion-at-point)))
  )

;; (ghci-compilation-bind-keys)
;; (ghci-compilation-hooks)

(provide 'ghci-compilation)
