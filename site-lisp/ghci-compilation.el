;;; ghci-compilation.el --- a GHCI comint bufferthat supports compile-goto-error -*- lexical-binding: t; -*-

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
(require 'project)
(require 'svg)

(defvar-local ghci-compilation-process-started nil)
(defvar-local ghci-compilation-has-loaded-prompt nil)
(defvar-local ghci-compilation-last-command-started-at nil)
(defvar-local ghci-compilation-last-prompt-marker nil)

(defun ghci-compilation-notify ()
  (require 'mode-line-notify)
  (let ((buffer (current-buffer)))
    (mode-line-notify-send :title "GHCi Ready" :action (lambda () (switch-to-buffer buffer))))
  ;; (require 'notifications)
  ;; (let (buf (current-buffer))
  ;;   (notifications-notify
  ;;    :title "GHCi has completed."
  ;;    :body "A GHCi process has completed. "
  ;;    :actions '("Goto GHCi")
  ;;    :on-action (lambda () (switch-to-buffer buffer))))
  )

(defcustom ghci-compilation-loaded-hook '(ghci-compilation-notify)
  "Called when ghci process has loaded."
  :type 'hook
  :group 'ghci-compilation)

(defcustom ghci-compilation-last-command-minimum-time 60
  "Minimum number of seconds a command took to be eligible to call
ghci-compilation-loaded-hook. Defaults to 60."
  :type 'number
  :group 'ghci-compilation)

(defcustom ghci-compilation-last-command-finished-hook '(ghci-compilation-notify)
  "Called when ghci has finished after a while."
  :type 'hook
  :group 'ghci-compilation)

(defun ghci-compilation-buffer-name (package)
  "Generate a name for ghci compilation buffer."
  (if-let (proj (project-current))
      (concat "*"
              "ghci-compilation"
              (when package (concat "-" package))
              "*"
              "<"
              (project-root (project-current))
              ">*")
    (error "No current project.")))

(defun ghci-compilation-get-buffer (package)
  "Try to find the ghci-compilation buffer."
  (get-buffer (ghci-compilation-buffer-name package))
  )

(defvar ghci-compilation-menu-map
  (let ((map (make-sparse-keymap "GHCi")))
    (define-key map [compilation-first-error]
                '(menu-item "First Error" first-error
		            :help "Restart at the first error, visit corresponding source code"))
    (define-key map [compilation-previous-error]
                '(menu-item "Previous Error" previous-error
		            :help "Visit previous `next-error' message and corresponding source code"))
    (define-key map [compilation-next-error]
                '(menu-item "Next Error" next-error
		            :help "Visit next `next-error' message and corresponding source code"))
    map))

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
    (define-key map "\M-\C-m" 'compile-goto-error)
    (define-key map "\M-\C-n" 'compilation-next-error)
    (define-key map "\M-\C-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)

    (define-key map [menu-bar ghci-compilation] (cons "GHCi" ghci-compilation-menu-map))

    map))

(define-derived-mode ghci-compilation-mode comint-mode "Ghci"
  "Major mode for Ghci Compilation Mode.")

(defun ghci-compilation--fetch-haskell-icon ()
  (let* ((filename "Haskell-Logo.svg")
         (url "https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg")
         (cache-dir (expand-file-name "ghci-compilation" (temporary-file-directory)))
         (cache-path (expand-file-name filename cache-dir)))
    (unless (file-exists-p cache-path)
      (make-directory cache-dir t)
      (let ((buffer (url-retrieve-synchronously url t t 5.0)))
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (re-search-forward "^HTTP/1.1 200 OK" nil t)
                (progn
                  (re-search-forward "\r?\n\r?\n")
                  (let ((coding-system-for-write 'no-conversion))
                    (write-region (point) (point-max) cache-path)))
              (message "Icon fetch failed: %s" url)))
          (kill-buffer buffer))))
    (when (file-exists-p cache-path)
      cache-path)))

(defun ghci-compilation--make-header ()
  (if (display-graphic-p)
      (let* ((image-height (* 3 (default-font-height)))
             (image-width image-height)
             (text-height 25)
             (icon-y 0)
             (icon-text-y (+ text-height 5))
             (total-height (+ image-height 10))
             (image-type "image/svg+xml")
             (svg (svg-create (frame-pixel-width) total-height)))
        (svg-embed svg (ghci-compilation--fetch-haskell-icon)
                   image-type nil
                   :x 0 :y icon-y :width image-width :height image-height)
        (svg--append svg (let ((text-node (dom-node 'text
                                                    `((x . ,(+ image-width 10))
                                                      (y . ,icon-text-y)))))
                           ;; Agent name
                           (dom-append-child text-node
                                             (dom-node 'tspan
                                                       `((fill . ,(face-attribute 'font-lock-variable-name-face :foreground)))
                                                       " GHCi"))
                           text-node))
        (format " %s" (with-temp-buffer
                        (svg-insert-image svg)
                        (buffer-string))))
    "====================== GHCi ======================"))

(defun ghci-compilation-input-sender (proc string)
  (unless (ghci-compilation--is-running-command)
    (setq-local ghci-compilation-last-command-started-at (current-time)))
  (comint-simple-send proc string))

(defun ghci-compilation--indicator ()
  ""
  (when (and ghci-compilation-last-command-started-at (ghci-compilation--is-running-command))
    '("running[" (:eval (format-time-string "%S" (time-subtract (current-time) ghci-compilation-last-command-started-at))) "] ")))

(defun ghci-compilation (&optional buffer flake-ref package ghci-repl-command)
  "Start ghci."
  (interactive
   (list
    nil
    "."
    nil
    nil))
  (unless buffer (setq buffer (get-buffer-create (ghci-compilation-buffer-name package))))
  (unless flake-ref (setq flake-ref (if (string-suffix-p "mercury-web-backend/" (project-root (project-current)))
                                        ".#exclude.all"
                                      ".")))
  (unless ghci-repl-command
    (setq ghci-repl-command (if (string-suffix-p "mercury-web-backend/" (project-root (project-current)))
                                '("mwb-ghci")
                              '("cabal" "repl"))))
  (let* ((proc-alive (comint-check-proc buffer))
         (buffer-env (append (list "PAGER=" (format "INSIDE_EMACS=%s,ghci-compilation" emacs-version)) (copy-sequence process-environment)))
         (proj (project-current)))
    (unless proc-alive
      (setq-local ghci-compilation-process-start t)

      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer))

        (ghci-compilation-mode)

        ;; performance
        (show-paren-mode -1)
        (setq-local bidi-inhibit-bpa nil)
        (setq-local bidi-paragraph-direction 'left-to-right)

        (setq-local header-line-format (ghci-compilation--make-header))

        (setq-local comint-terminfo-terminal "xterm-256color")
        (setq-local comint-prompt-regexp "^ghci> ")
        (setq-local comint-prompt-read-only t)
        (setq-local comint-use-prompt-regexp t)
        (setq-local comint-process-echoes nil)
        (setq-local comint-input-sender 'ghci-compilation-input-sender)
        (setq-local process-environment buffer-env)
        (setq-local default-directory (project-root proj))

        (setq-local ghci-compilation-has-loaded-prompt nil)
        (setq-local ghci-compilation-last-prompt-marker (make-marker))

        (setq-local comint-input-ring-file-name (expand-file-name "ghci-compilation-history" user-emacs-directory))

        (make-local-variable 'mode-line-misc-info)
        (add-to-list 'mode-line-misc-info '(:eval (ghci-compilation--indicator)))

        (apply 'make-comint-in-buffer "ghci-compilation" buffer "nix" nil "develop" flake-ref "-c" ghci-repl-command)

        (comint-read-input-ring 'silent)

        (compilation-setup t)

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

(defun ghci-compilation--send-string (text &optional buffer)
  "Send `TEXT' to the inferior Haskell REPL process"
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
  (unless (with-current-buffer buffer ghci-compilation-has-loaded-prompt)
    (error "GHCI is starting up"))
  (let ((process (get-buffer-process buffer)))
    (comint-send-string process (concat text "\n"))))

(defun ghci-compilation-revert-buffer (_ignore-auto _noconfirm)
  "Revert buffer"
  (if (comint-check-proc (current-buffer))
      (ghci-compilation-reload)
    (ghci-compilation (current-buffer))))

(defun ghci-compilation-preoutput-filter (text)
  "Make output read only."
  text
  ;; (propertize text 'read-only t)
  )

(defun ghci-compilation--is-prompt ()
  (save-excursion
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (forward-line 0)
    (looking-at comint-prompt-regexp)))

(defun ghci-compilation-process-output (&optional _)
  "Process output to check if ghci has loaded."
  (when (ghci-compilation--is-prompt)
    (when-let* ((prompt-marker (if comint-last-prompt (car comint-last-prompt) comint-last-output-start)))
      (set-marker ghci-compilation-last-prompt-marker prompt-marker))
    (when ghci-compilation-has-loaded-prompt
      (run-hooks 'ghci-compilation-last-command-finished-hook))
    (setq-local ghci-compilation-has-loaded-prompt t)
    (when (or (not ghci-compilation-last-command-minimum-time)
              (time-less-p ghci-compilation-last-command-minimum-time (time-subtract (current-time) ghci-compilation-last-command-started-at)))
      (run-hooks 'ghci-compilation-last-command-finished-hook))))

(defun ghci-compilation-save-files ()
  "Save files in project."
  (interactive)
  (let ((proj (project-current)))
    (save-some-buffers t (and proj (memq (current-buffer) (project-buffers proj))))))

(defun ghci-compilation-reload (&optional buffer)
  "Reload ghci."
  (interactive)
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
  (with-current-buffer buffer
    (if ghci-compilation-has-loaded-prompt
        (progn
          (when compilation-locs (compilation-forget-errors))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (ghci-compilation-save-files)
          (ghci-compilation--send-string ":r" buffer))
      (ghci-compilation))))

(defun ghci-compilation-send-command-redirect (command &optional buffer)
  "Send command to ghci, outputing as string."
  (interactive)
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
  (unless (with-current-buffer buffer ghci-compilation-has-loaded-prompt)
    (error "GHCI is starting up"))
  (let* ((inhibit-quit t)
         (lines (comint-redirect-results-list-from-process (get-buffer-process buffer) command "^\\(.*\\)$" 1)))
    (mapconcat 'identity lines "\n")))

(defun ghci-compilation--is-running-command ()
  (and ghci-compilation-has-loaded-prompt
       (and (marker-position ghci-compilation-last-prompt-marker)
            (< ghci-compilation-last-prompt-marker comint-last-output-start))))

(defun ghci-compilation-get-last-output--marker (&optional buffer)
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
  (with-current-buffer buffer
    (when ghci-compilation-has-loaded-prompt
      (error "currently waiting on response from ghci"))
    (let* ((end (or (car comint-last-prompt) comint-last-input-start (point-min)))
           (beg (save-excursion (goto-char end) (if (re-search-backward comint-prompt-regexp (point-min) t) (match-beginning 0) (point-min)))))
      (list buffer beg end))))

(defun ghci-compilation-get-last-output (&optional buffer)
  "Get output of last command."
  (let* ((res (ghci-compilation-get-last-output--marker buffer))
         (buffer (nth 0 res))
         (beg (nth 1 res))
         (end (nth 2 res)))
    (with-current-buffer buffer (buffer-substring-no-properties beg end))))

(defun ghci-compilation-get-module-name ()
  "Get the module name from a filename. Cabal is stupid & can’t tell when these are the same."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^module \\([a-zA-Z.]+\\)[( ]" nil t)
    (match-string-no-properties 1)))

(defun ghci-compilation-add-file ()
  "Add file to ghci."
  (interactive)
  (let ((module-name (ghci-compilation-get-module-name)))
    (ghci-compilation-save-files)
    (ghci-compilation--send-string (format ":add %s" module-name))))

(defun ghci-compilation-test-file ()
  "Add file to ghci."
  (interactive)
  (let ((module-name (ghci-compilation-get-module-name))
        (is-web (save-excursion
                  (goto-char (point-min))
                  (re-search-forward ":: *SpecWeb" nil t))))
    (ghci-compilation-save-files)
    (ghci-compilation--send-string (concat
                                    (format ":add %s\n" module-name)
                                    (if is-web (format ":import-spec-web\nimport %s\nhspecWithEnv spec\n" module-name) "hspec spec\n")))))

(defun ghci-compilation-reload-nix (&optional buffer)
  "Reload nix too."
  (interactive)
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
  (with-current-buffer buffer
    (unless ghci-compilation-has-loaded-prompt
      (error "GHCI is starting up"))
    (comint-write-input-ring)
    (let ((proc (get-buffer-process buffer))
          (inhibit-read-only t))
      (and proc (delete-process proc)))
    (ghci-compilation buffer)))

(defun ghci-compilation-load-all (&optional buffer)
  "Load all modules."
  (interactive)
  (ghci-compilation--send-string ":add Application TestMain\n" buffer))

(defun ghci-compilation-switch-to-buffer ()
  ""
  (interactive)

  (let ((buffer (ghci-compilation-get-buffer nil)))
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

(defun ghci-compilation-send-input (&optional _no-newline _artificial)
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
  (if (and comint-redirect-output-buffer (not comint-redirect-completed))
      (progn
        (warn "waiting on completion of last command")
        nil)
    (let* ((cmd (format ":complete repl 100 \"%s\"" prefix))
           (lines (let ((inhibit-quit nil)) (comint-redirect-results-list-from-process (get-buffer-process buf) cmd "^\\(.*\\)$" 1)))
           (header (split-string-and-unquote (nth 0 lines) " "))
           (num-results (string-to-number (nth 0 header)))
           (prefix (nth 2 header))
           (completions nil))
      (dotimes (i num-results)
        (push (concat prefix (nth 0 (split-string-and-unquote (nth (+ 1 i) lines) " "))) completions))
      completions)))

(defun ghci-compilation-complete-at-point ()
  "Complete at point using ghci’s :complete command."
  (when-let* ((start (comint-line-beginning-position))
              (end (point)))
    (when (> end start)
      (list start end (ghci-compilation--completions (current-buffer) (buffer-substring-no-properties start end)) nil))))

(defun ghci-compilation-haskell-completion-at-point (&optional buffer)
  (unless buffer
    (setq buffer (ghci-compilation-get-buffer nil)))
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
                    (when ghci-compilation-has-loaded-prompt
                      (ghci-compilation--completions buffer (if is-import
                                                                (concat "import " pfx)
                                                              pfx))
                      )))
              (when is-import
                (setq candidates (mapcar (lambda (candidate) (string-remove-prefix "import " candidate)) candidates)))
              (list beg end candidates))))))
     (is-blank-import ;; special support for just "import " completion (no module name)
      (let ((candidates
             (when ghci-compilation-has-loaded-prompt
               (ghci-compilation--completions buffer "import ")
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
