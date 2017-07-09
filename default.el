;;; default -- @matthewbauer’s Emacs config

;;; Commentary:

;;; This should be used in conjunction with Nixpkgs in config.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package apropospriate-theme
  :demand
  :config (load-theme 'apropospriate-dark t))

(setenv "NIX_SSL_CERT_FILE" "/etc/ssl/certs/ca-bundle.crt")
(setenv "EDITOR" "emacsclient -nw")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "PAGER" "cat")

(defun set-defaults (&rest args)
  "Set defaults in the same way as ’custom-set-variables’.
ARGS are a list in the form of (SYMBOL VALUE)."
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry))))
      (unless (or (get symbol 'standard-value)
                  (memq (get symbol 'custom-autoload) '(nil noset)))
        ;; This symbol needs to be autoloaded, even just for a `set'.
        (custom-load-symbol symbol))))
  (dolist (entry args)
    (let* ((symbol (indirect-variable (nth 0 entry)))
           (value (nth 1 entry))
           (now (nth 2 entry))
           (requests (nth 3 entry))
           (comment (nth 4 entry)))
      ;; (custom-push-theme 'theme-value symbol 'user 'set value)
      (when requests
        (put symbol 'custom-requests requests)
        (mapc 'require requests))
      (setq set (or (get symbol 'custom-set) 'custom-set-default))
      ;; (put symbol 'default-value (list value))
      ;; (put symbol 'saved-value (list value))
      (put symbol 'standard-value (list value))
      ;; (put symbol 'force-value t)
      (funcall set symbol (eval value)))))

(set-defaults
 '(ad-redefinition-action (quote accept))
 '(ag-executable "@ag@/bin/ag")
 '(apropos-do-all t)
 '(async-shell-command-buffer (quote new-buffer))
 '(auth-source-save-behavior t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-verbose nil)
 '(auto-save-visited-file-name t)
 '(async-shell-command-buffer 'new-buffer)
 '(backward-delete-char-untabify-method (quote hungry))
 '(bm-buffer-persistence t)
 '(bm-restore-repository-on-load t)
 '(bm-cycle-all-buffers t)
 '(c-syntactic-indentation nil)
 '(column-number-mode t)
 '(comint-scroll-show-maximum-output nil)
 '(company-auto-complete nil)
 '(company-continue-commands
   (quote
    (not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs comint-previous-matching-input-from-input comint-next-matching-input-from-input)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-occurrence-weight-function (quote company-occurrence-prefer-any-closest))
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-limit 10)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-environment '("TERM=xterm-256color"))
 '(completions-format (quote vertical))
 '(completion-cycle-threshold 5)
 '(counsel-find-file-at-point t)
 '(counsel-mode-override-describe-bindings t)
 '(create-lockfiles nil)
 '(custom-buffer-done-kill t)
 '(custom-search-field nil)
 '(create-lockfiles nil)
 '(debug-ignored-errors
   (quote
    ("^Invalid face:? " search-failed beginning-of-line beginning-of-buffer end-of-line end-of-buffer end-of-file buffer-read-only file-supersession mark-inactive user-error void-variable)))
 '(debug-on-signal t)
 '(dired-dwim-target t)
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(dtrt-indent-verbosity 0)
 '(display-buffer-alist
   (\`
    (((\,
       (rx bos
           (or "*Flycheck errors*" "*Backtrace" "*Warnings" "*compilation" "*Help" "*less-css-compilation" "*Packages" "*magit-process" "*SQL" "*tldr")))
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (reusable-frames . visible)
      (window-height . 0.33))
     ("." nil
      (reusable-frames . visible)))))
 '(display-buffer-reuse-frames t)
 '(dumb-jump-quiet t)
 '(dumb-jump-selector (quote ivy))
 '(enable-recursive-minibuffers nil)
 '(epg-gpg-program "@gpg@/bin/gpg")
 '(epg-gpgconf-program "@gpg@/bin/gpgconf")
 '(epg-gpgsm-program "@gpg@/bin/gpgsm")
 '(erc-autoaway-idle-seconds 600)
 '(erc-autoaway-use-emacs-idle t)
 '(erc-autojoin-timing (quote ident))
 '(erc-fill-prefix "          ")
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display (quote buffer))
 '(erc-server-coding-system (quote (utf-8 . utf-8)))
 '(erc-timestamp-format "%H:%M ")
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-track-exclude-types
   (quote
    ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-try-new-nick-p nil)
 '(eshell-banner-message "")
 '(eshell-buffer-maximum-lines 20000)
 '(eshell-buffer-shorthand t)
 '(eshell-cd-on-directory t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-cmpl-cycle-cutoff-length 2)
 '(eshell-cmpl-ignore-case t)
 '(eshell-cp-interactive-query t)
 '(eshell-cp-overwrite-files nil)
 '(eshell-default-target-is-dot t)
 '(eshell-destroy-buffer-when-process-dies t)
 '(eshell-highlight-prompt nil)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 10000)
 '(eshell-list-files-after-cd t)
 '(eshell-ln-interactive-query t)
 '(eshell-mv-interactive-query t)
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt eshell-truncate-buffer)))
 '(eshell-plain-echo-behavior nil)
 '(eshell-review-quick-commands t)
 '(eshell-rm-interactive-query t)
 '(eshell-visual-commands
   (quote
    ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"
     "nano" "nethack" "telnet" "emacs" "emacsclient" "htop" "w3m" "links" "lynx"
     "elinks" "irrsi" "mutt" "finch" "newsbeuter" "pianobar")))
 '(eshell-visual-options (quote (("git" "--paginate"))))
 '(eshell-visual-subcommands (quote (("git" "log" "diff" "show"))))
 '(eval-expression-debug-on-error t)
 '(explicit-bash-args (quote ("-c" "export EMACS= INSIDE_EMACS=; stty echo; bash")))
 '(explicit-shell-file-name "bash")
 '(expand-region-contract-fast-key "j")
 '(fased-completing-read-function (quote nil))
 '(fill-column 80)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
 '(flycheck-standard-error-navigation nil)
 '(flycheck-global-modes
   (quote
    (not erc-mode message-mode git-commit-mode view-mode outline-mode text-mode org-mode)))
 '(flyspell-abbrev-p nil)
 '(flyspell-auto-correct nil)
 '(flyspell-highlight-properties nil)
 '(flyspell-incorrect-hook nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil)
 '(fortune-dir "@fortune@/share/games/fortunes")
 '(fortune-file "@fortune@/share/games/fortunes/food")
 '(frame-title-format
   (quote
    ((:eval
      (if (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b")))))
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-non-file-buffers t)
 '(hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
 '(hideshowvis-ignore-same-line nil)
 '(history-delete-duplicates t)
 '(history-length 20000)
 '(iedit-toggle-key-default nil)
 '(imenu-auto-rescan t)
 '(imap-ssl-program '("@gnutls@/bin/gnutls-cli --tofu -p %p %s"))
 '(indicate-empty-lines t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-echo-area-message t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "")
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-silently-savep t)
 '(ispell-quietly t)
 '(ispell-program-name "@aspell@/bin/aspell")
 '(ivy-count-format "\"\"")
 '(ivy-display-style nil)
 '(ivy-minibuffer-faces nil)
 '(ivy-use-virtual-buffers t)
 '(jdee-server-dir "@jdeeserver@")
 '(jdee-ant-home "@ant@/lib/ant")
 '(jdee-ant-program "@ant@/bin/ant")
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(load-prefer-newer t)
 '(mac-command-key-is-meta t)
 '(mac-command-modifier 'meta)
 '(mac-option-key-is-meta nil)
 '(mac-option-modifier 'super)
 '(mac-right-option-modifier nil)
 '(mac-frame-tabbing t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-clone-set-remote\.pushDefault t)
 '(magit-completing-read-function 'ivy-completing-read)
 '(magit-delete-by-moving-to-trash t)
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-no-confirm t)
 '(magit-process-connection-type nil)
 '(magit-process-find-password-functions (quote (magit-process-password-auth-source)))
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:
  ")))
 '(magit-save-repository-buffers (quote dontask))
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(make-backup-files nil)
 '(minibuffer-prompt-properties
   (quote
    (read-only t cursor-intangible t face minibuffer-prompt)))
 '(mmm-global-mode 'buffers-with-submode-classes)
 '(mmm-submode-decoration-level 2)
 '(network-security-level (quote medium))
 '(neo-theme 'arrow)
 '(neo-fixed-size nil)
 '(next-error-recenter (quote (4)))
 '(nrepl-log-messages t)
 '(nsm-save-host-names t)
 '(org-support-shift-select t)
 '(parens-require-spaces t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")))
 '(package-enable-at-startup nil)
 '(parse-sexp-ignore-comments t)
 '(proof-splash-enable nil)
 '(projectile-globally-ignored-files (quote (".DS_Store" "TAGS")))
 '(projectile-enable-caching t)
 '(projectile-enable-idle-timer nil)
 '(projectile-mode-line
   '(:eval (if (and
                (projectile-project-p)
                (not (file-remote-p default-directory)))
               (format " Projectile[%s]" (projectile-project-name))
             "")))
 '(projectile-ignored-project-function (quote file-remote-p))
 '(projectile-switch-project-action (quote projectile-dired))
 '(projectile-verbose nil)
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(resize-mini-windows t)
 '(ring-bell-function (quote ignore))
 '(ripgrep-executable "@ripgrep@/bin/rg")
 '(rtags-path "@rtags@/bin")
 '(rtags-completions-enabled t)
 '(rtags-display-result-backend (quote ivy))
 '(rtags-imenu-syntax-highlighting 10)
 '(ruby-insert-encoding-magic-comment nil)
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables (quote (search-ring regexp-search-ring)))
 '(savehist-autosave-interval 60)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode)))
 '(shell-completion-execonly nil)
 '(shell-input-autoexpand nil)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-hybrid-kill-entire-symbol nil)
 '(tab-always-indent 'complete)
 '(term-input-autoexpand t)
 '(term-input-ignoredups t)
 '(term-input-ring-file-name t)
 '(tramp-default-proxies-alist
   (quote
    (((regexp-quote
       (system-name))
      nil nil)
     (nil "\\`root\\'" "/ssh:%h:")
     (".*" "\\`root\\'" "/ssh:%h:"))))
 '(tramp-remote-path
   (quote
    (tramp-own-remote-path "/run/current-system/sw/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin")))
 '(tls-checktrust t)
 '(tls-program '("@gnutls@/bin/gnutls-cli --tofu -p %p %h"))
 '(undo-limit 800000)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'forward)
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-ensure-function (quote ignore))
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(use-package-verbose nil)
 '(vc-allow-async-revert t)
 '(vc-command-messages nil)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-ignore-dir-regexp
   "\\(\\(\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\)\\|\\(\\`/[^/|:][^/|]*:\\)\\)\\|\\(\\`/[^/|:][^/|]*:\\)")
 '(view-read-only t)
 '(view-inhibit-help-message t)
 '(visible-bell nil)
 '(visible-cursor nil)
 '(whitespace-line-column 80)
 '(whitespace-auto-cleanup t)
 '(whitespace-rescan-timer-time nil)
 '(whitespace-silent t)
 '(whitespace-style '(face trailing lines space-before-tab empty lines-style))
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 )

(use-package hook-helpers
  :commands (create-hook-helper define-hook-helper))

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten y or n confirm

;; these should be disabled before Emsacs displays the frame
;; but we disable them here just in case
(when (and (fboundp 'menu-bar-mode)
           (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

(when (eq system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  )

;; (require 'server)
;; (when (not server-process)
;;   (server-start))

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defsubst hook-into-modes (func &rest modes)
  "Add hook to modes.
FUNC is run when MODES are loaded."
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; (define-hook-helper prog-mode ()
;;   "Highlight a bunch of well known comment annotations.

;; This functions should be added to the hooks of major modes for programming."
;;   (font-lock-add-keywords
;;    nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
;;           1 font-lock-warning-face t)))
;;   )

;; Show trailing whitespace
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
;; (create-hook-helper no-trailing-whitespace ()
;;   :hooks (special-mode-hook
;;           eshell-mode-hook
;;           eww-mode
;;           term-mode-hook
;;           comint-mode-hook
;;           compilation-mode-hook
;;           twittering-mode-hook
;;           minibuffer-setup-hook)
;;   (setq-local show-trailing-whitespace nil))

;;
;; key binds
;;

;; unbind unused keys
(global-unset-key "\C-z") ; don’t suspend on C-z
(global-unset-key [?\s-p]) ; printing crashes occasionally
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c C-u") 'rename-uniquely)
(global-set-key (kbd "C-x ~") (lambda () (interactive) (find-file "~")))
(global-set-key (kbd "C-x /") (lambda () (interactive) (find-file "/")))
(global-set-key (kbd "C-c l") 'browse-url-at-point)
(global-set-key (kbd "C-x 5 3") 'iconify-frame)
(global-set-key (kbd "C-x v f") 'vc-git-grep)
(global-set-key (kbd "s-SPC") 'cycle-spacing)
(global-set-key (kbd "C-c v") 'customize-variable)

(global-unset-key (kbd "C-x C-e"))
(create-hook-helper always-eval-sexp ()
  :hooks (lisp-mode-hook emacs-lisp-mode-hook)
  (define-key (current-local-map) (kbd "C-x C-e") 'eval-last-sexp))

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)))

(defun sort-package-declarations ()
  "Sort following package declarations alphabetically."
  (interactive)
  (cl-flet ((next-use-package
             () (if (re-search-forward "^(use-package " nil t)
                    (goto-char (match-beginning 0))
                  (goto-char (point-max)))))
    (sort-subr
     nil
     #'next-use-package
     #'(lambda ()
         (goto-char (line-end-position))
         (next-use-package))
     #'(lambda ()
         (re-search-forward "(use-package \\([A-Za-z0-9_+-]+\\)")
         (match-string 1)))))

(use-package abbrev
  :diminish abbrev-mode
  :demand
  :config (setq-default abbrev-mode t))

(use-package ace-window
  :bind (("M-o" . other-window)
         ("C-x o" . ace-window)))

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp)
  :bind ("C-?" . ag-project))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init (apply #'hook-into-modes 'aggressive-indent-mode lisp-mode-hooks))

(use-package align
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark))))
  )

(use-package ansi-color
  :init (create-hook-helper colorize-compilation-buffer ()
          :hooks (compilation-filter-hook)
          (when (eq major-mode 'compilation-mode)
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))))

(use-package autorevert
  :commands global-auto-revert-mode
  :defer 4
  :config (global-auto-revert-mode t))

(use-package browse-at-remote
  :commands browse-at-remote)

(use-package buffer-move
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package bury-successful-compilation
  :disabled
  :commands bury-successful-compilation
  :init (bury-successful-compilation 1))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.c\\'"                   . c-mode)
         ("\\.cpp\\'"                 . c++-mode)
         ("\\.c++\\'"                 . c++-mode)
         ("\\.mm\\'"                  . c++-mode))
  :config
  (use-package c-eldoc
    :commands c-turn-on-eldoc-mode
    :init (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))
  )

(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)))

(use-package company
  :preface
  (defun my-complete ()
    (interactive)
    (cond ((eq major-mode 'jdee-mode) (jdee-complete-menu)
           (t (company-complete-common-or-cycle)))))

  :bind (("<C-tab>" . my-complete)
         ("M-C-/" . company-complete))
  :diminish company-mode
  :commands (company-mode global-company-mode company-complete-common)
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-irony
  :disabled
  :commands company-irony
  :after company
  :init (add-to-list 'company-backends 'company-irony))

(use-package compile
  :bind (("C-c C-c" . compile)
         ("M-O"   . show-compilation))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*compilation\\*" (buffer-name buf))
                   (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config

  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)
  )

(use-package counsel
  :commands (counsel-descbinds)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("<f1> l" . counsel-find-library)
         ;; ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
	 ("M-y" . counsel-yank-pop)))

(use-package counsel-dash
  :commands counsel-dash)

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-toggle 1))

(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-e" . crux-eval-and-replace)
         ([shift return] . crux-smart-open-line)))

(use-package css-mode
  :mode "\\.css\\'"
  :commands css-mode
  :config
  (use-package css-eldoc
    :demand)
  )

(use-package diff-hl
  :commands (diff-hl-dir-mode diff-hl-mode diff-hl-magit-post-refresh
                              diff-hl-diff-goto-hunk)
  :bind (:map diff-hl-mode-map
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :init
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dir-mode)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package dired
  :bind (("C-c J" . dired-double-jump)
         :map dired-mode-map
         (("C-c C-c" . compile)
          ("r" . browse-url-of-dired-file)
          ("M-!" . async-shell-command)))
  )

(use-package dired-x
  :commands dired-omit-mode
  :disabled
  :init
  ;; toggle `dired-omit-mode' with C-x M-o
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  )

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :defer 3
  :config (dtrt-indent-mode 1))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (dumb-jump-mode))

(use-package edebug)

(use-package eldoc
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :config
  (defalias 'eldoc-get-fnsym-args-string 'elisp-get-fnsym-args-string)
  )

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode))

(use-package emacs-lisp-mode
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package erc
  :bind ("C-x r c" . erc))

(use-package esh-help
  :commands esh-help-eldoc-command
  :init (create-hook-helper esh-help-setup ()
          :hooks (eshell-mode-hook)
          (make-local-variable 'eldoc-documentation-function)
          (setq eldoc-documentation-function 'esh-help-eldoc-command)
          (eldoc-mode)))

(use-package eshell
  :bind (("C-c s" . eshell))
  :commands (eshell eshell-command))

(use-package eshell-extras
  :after eshell)

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :init (setq eshell-prompt-function 'epe-theme-lambda))

(use-package ess-site
  :commands R)

(use-package esup
  :commands esup)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flycheck
  :commands global-flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :commands flycheck-irony-setup
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package ggtags)

(use-package ghc)

(use-package gist
  :bind ("C-c C-g" . gist-region-or-buffer-private)
  :commands (gist-list gist-region gist-region-private gist-buffer
                       gist-buffer-private gist-region-or-buffer
                       gist-region-or-buffer-private))

(use-package gnus
  :commands gnus
  :bind (("C-M-g" . gnus) ("C-x n u" . gnus))
  :init
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :mode "\\.go\\'")

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)))

(use-package gud
  :commands gud-gdb
  :bind (("C-. g" . show-debugger)
         ("<f9>" . gud-cont)
         ("<f10>" . gud-next)
         ("<f11>" . gud-step)
         ("S-<f11>" . gud-finish)))

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package hideshow
  :commands hs-minor-mode
  :init (apply #'hook-into-modes 'hs-minor-mode '(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook java-mode-hook)))

(use-package hideshowvis
  :disabled
  :commands (hideshowvis-minor-mode hideshowvis-symbols)
  :init
  (add-hook 'prog-mode-hook 'hideshowvis-minor-mode)
  )

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hydra
  :disabled
  :bind (("s-f" . hydra-projectile/body)
         ("C-x t" . hydra-toggle/body)
         ("C-M-o" . hydra-window/body))
  :config
  (hydra-add-font-lock)
  )

(use-package ibuffer
  :bind ("C-x b" . ibuffer))

(use-package iedit
  :bind (("C-;" . iedit-mode)
         :map help-map ("C-;" . iedit-mode-toggle-on-function)
         :map esc-map ("C-;" . iedit-mode-toggle-on-function)
         :map isearch-mode-map ("C-;" . iedit-mode-toggle-on-function))
  )

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package imenu-list
  :commands imenu-list)

(use-package indium
  :mode ("\\.js\\'" . indium-mode)
  :commands (indium-mode indium-interaction-mode indium-scratch))

(use-package intero
  :commands intero-mode
  :init (add-hook 'haskell-mode-hook 'intero-mode))

(use-package irony
  :commands irony-mode
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  )

(use-package irony-eldoc
  :commands irony-eldoc
  :init (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package ivy
  :after projectile
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :init (setq projectile-completion-system (quote ivy))
  :config (ivy-mode 1))

(use-package jdee
  :mode ("\\.java\\'" . jdee-mode)
  :commands jdee-mode
  :bind (:map jdee-mode-map
              ("<s-mouse-1>" . jdee-open-class-at-event)))

(use-package js2-mode
  :config
  (js2-imenu-extras-setup))

(use-package json-mode)

(use-package keyfreq
  :disabled
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind (("C-x k" . kill-or-bury-alive)
         ("C-c r" . kill-or-bury-alive-purge-buffers)))

(use-package lisp-mode)

(use-package lsp-mode
  :commands lsp-mode
  :init (add-hook 'prog-major-mode #'lsp-mode)

  :config

  (use-package lsp-java
    :demand)
  (use-package lsp-haskell
    :demand)
  (use-package lsp-go
    :demand)
  (use-package lsp-python
    :demand)
  (use-package lsp-rust
    :demand)
  )

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package magit
  :commands (magit-clone)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup)))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :commands markdown-mode)

(use-package minimap
  :commands minimap-mode)

(use-package mmm-mode
  :disabled
  :demand
  :config
  (use-package mmm-auto
    :demand))

(use-package move-text
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package multiple-cursors
  :bind
  (("<C-S-down>" . mc/mark-next-like-this) ;; broken by macOS shortcut
   ("<C-S-up>" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package neotree
  :bind (("<f8>" . neotree-toggle)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package notmuch
  :commands notmuch)

(use-package org
  ;; :mode "\\.\\(org\\)\\'"
  :commands org-capture
  :bind ("C-c c" . org-capture)
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (gnuplot . t)
     (dot . t)
     (ditaa . t)
     (R . t)
     (python . t)
     (ruby . t)
     (js . t)
     (clojure . t)
     (sh . t)))
  (use-package ox-latex
    :demand)
  )

(use-package org-bullets
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package page-break-lines
  :commands page-break-lines-mode
  :init
  (add-hook 'doc-mode-hook 'page-break-lines-mode)
  (add-hook 'help-mode-hook 'page-break-lines-mode)
  (add-hook 'emacs-lisp-mode-hook 'page-break-lines-mode)
  )

(use-package paren
  :demand
  :disabled
  :config (show-paren-mode 1))

(use-package php-mode
  :mode "\\.php\\'")

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config

  (put 'projectile-project-compilation-cmd 'safe-local-variable
       (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command))
                                        compilation-read-command))))

  (projectile-global-mode)

  (use-package easymenu
    :demand
    :config

    ;; just make mode line clickable
    (easy-menu-define projectile-menu projectile-mode-map "Projectile"
      '("Projectile"
        :active nil ;; disable menu bar
        ["Find file" projectile-find-file]
        ["Find file in known projects" projectile-find-file-in-known-projects]
        ["Find test file" projectile-find-test-file]
        ["Find directory" projectile-find-dir]
        ["Find file in directory" projectile-find-file-in-directory]
        ["Find other file" projectile-find-other-file]
        ["Switch to buffer" projectile-switch-to-buffer]
        ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
        ["Kill project buffers" projectile-kill-buffers]
        ["Recent files" projectile-recentf]
        ["Edit .dir-locals.el" projectile-edit-dir-locals]
        "--"
        ["Open project in dired" projectile-dired]
        ["Switch to project" projectile-switch-project]
        ["Switch to open project" projectile-switch-open-project]
        ["Discover projects in directory" projectile-discover-projects-in-directory]
        ["Search in project (grep)" projectile-grep]
        ["Search in project (ag)" projectile-ag]
        ["Replace in project" projectile-replace]
        ["Multi-occur in project" projectile-multi-occur]
        ["Browse dirty projects" projectile-browse-dirty-projects]
        "--"
        ["Run shell" projectile-run-shell]
        ["Run eshell" projectile-run-eshell]
        ["Run term" projectile-run-term]
        "--"
        ["Cache current file" projectile-cache-current-file]
        ["Invalidate cache" projectile-invalidate-cache]
        ["Regenerate [e|g]tags" projectile-regenerate-tags]
        "--"
        ["Compile project" projectile-compile-project]
        ["Test project" projectile-test-project]
        ["Run project" projectile-run-project]
        "--"
        ["Project info" projectile-project-info]
        ["About" projectile-version]
        )
      )
    )

  )

(use-package proof-site
  :demand)

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (apply #'hook-into-modes 'rainbow-delimiters-mode lisp-mode-hooks))

(use-package rainbow-mode
  :commands rainbow-mode
  :init (apply #'hook-into-modes 'rainbow-mode '(css-mode-hook html-mode-hook sass-mode-hook)))

(use-package realgud
  :commands (realgud:jdb))

(use-package restart-emacs
  :commands restart-emacs)

(use-package rg
  :commands rg)

(use-package rtags
  :commands (rtags-start-process-unless-running rtags-enable-standard-keybindings)
  :init

  ;; Start rtags upon entering a C/C++ file
  (create-hook-helper rtags-start ()
    :hooks (c-mode-common-hook c++-mode-common-hook)
    (when (not (tramp-tramp-file-p (buffer-file-name (current-buffer))))
      (rtags-start-process-unless-running)))

  :config
  ;; Keybindings
  (rtags-enable-standard-keybindings c-mode-base-map "\C-cr"))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package savehist
  :defer 4
  :commands savehist-mode
  :config (savehist-mode 1))

(use-package saveplace
  :commands save-place-mode
  :defer 5
  :config (save-place-mode t))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package semantic
  :disabled
  )

(use-package sh-script
  :preface
  (defun shell-command-at-point ()
    (interactive)
    (setq start-point (save-excursion
                        (beginning-of-line)
                        (point)))
    (shell-command (buffer-substring start-point (point)))
    )
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode))
  :bind (:map sh-mode-map
              ("C-x C-e" . shell-command-at-point))
  )

(use-package shell
  :commands (shell shell-mode)
  :bind ("C-c C-s" . shell)
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )

(use-package shell-script-mode
  :commands shell-script-mode
  :mode (("\\.zsh\\'" . shell-script-mode)))

(use-package smart-hungry-delete
  :disabled
  :commands smart-hungry-delete-add-default-hooks
  :bind (:map prog-mode-map
              ("<backspace>" . smart-hungry-delete-backward-char)
              ("C-d" . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package smart-tabs-mode
  :init (add-hook 'prog-mode-hook 'smart-tabs-mode)
  :commands smart-tabs-mode)

(use-package smartparens
  :commands smartparens-mode
  :init
  (setq sp-base-key-bindings (quote paredit))
  (apply #'hook-into-modes 'smartparens-mode lisp-mode-hooks)
  (apply #'hook-into-modes 'show-smartparens-mode lisp-mode-hooks)
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  :config
  (use-package smartparens-config
    :demand)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  )

(use-package sudo-edit
  :bind (("C-c C-r" . sudo-edit)))

(use-package swiper
  :bind (("\C-s" . swiper)
         ("\C-r" . swiper)))

(use-package term
  :preface
  (defun my-term ()
    (interactive)
    (set-buffer (make-term "my-term" "zsh"))
    (term-mode)
    ;; (term-line-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer "*my-term*"))
  :bind ("C-c t" . my-term)
  :init

  (defun nethack ()
    (interactive)
    (set-buffer (make-term "nethack" "@nethack@/bin/nethack"))
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer "*nethack*"))
  )

(use-package tern
  :commands tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package try
  :commands try)

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)))

(use-package which-func
  :commands which-function-mode
  :defer 1
  :config (which-function-mode t))

(use-package which-key
  :commands which-key-mode
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package windmove
  :defer 3
  :commands windmove-default-keybindings
  :config (windmove-default-keybindings 'meta))

(use-package xterm-color
  :defer 5
  :config
  ;; Comint and Shell
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))

  (define-hook-helper compilation-start (proc)
    (when (eq (process-filter proc) 'compilation-filter)
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string)))))
    )
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package elec-pair
  :commands electric-pair-mode
  :defer 6
  :config (electric-pair-mode t))

(use-package delsel
  :defer 5
  :config (delete-selection-mode t))

(use-package jka-compr
  :defer 6
  :config
  ;; binary plist support
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  (jka-compr-update)
  )

(use-package time
  :commands display-time
  :config (display-time))

(use-package electric
  :demand
  :config
  (electric-quote-mode t)
  (electric-indent-mode t)
  )

(use-package prog-mode
  :commands global-prettify-symbols-mode
  :init
  (global-prettify-symbols-mode)
  )

(use-package with-editor
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command))
  :commands with-editor-export-editor
  :init
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  )

(provide 'default)
;;; default.el ends here
