;;; default -- @matthewbauer’s Emacs config

;;; Commentary:

;;; This should be used in conjunction with Nixpkgs in config.

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; (setenv "NIX_REMOTE" "daemon")
(setenv "NIX_SSL_CERT_FILE" "/etc/ssl/certs/ca-bundle.crt")
(setenv "EDITOR" "emacsclient -nw")
;; (setenv "PATH" "~/.nix-profile/bin")
;; (setenv "MANPATH" "~/.nix-profile/share/man")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

(defun set-defaults (&rest args)
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
      (funcall set symbol (eval value))
      )))

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
 '(c-eldoc-includes "")
 '(c-syntactic-indentation nil)
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
 '(desktop-dirname (concat user-emacs-directory "desktop"))
 '(dired-dwim-target t)
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'top)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
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
 '(projectile-completion-system (quote ivy))
 '(projectile-globally-ignored-files (quote (".DS_Store" "TAGS")))
 '(projectile-enable-caching t)
 '(projectile-enable-idle-timer t)
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
 '(vc-command-messages t)
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

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten y or n confirm

(defalias 'eldoc-get-fnsym-args-string 'elisp-get-fnsym-args-string)

;;
;; builtins
;;
(electric-pair-mode t)
(electric-quote-mode t)
(electric-indent-mode t)
;; (show-paren-mode 1)
;; (winner-mode t)
(which-function-mode t)
(blink-cursor-mode 0)
;; (save-place-mode 1)
(delete-selection-mode t)
;; (savehist-mode 1)
(column-number-mode t)
;; (transient-mark-mode 1)
;; (temp-buffer-resize-mode 0)
;; (minibuffer-depth-indicate-mode t)

(global-auto-revert-mode t)
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(when (and (fboundp 'menu-bar-mode)
           (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(display-time)
(prefer-coding-system 'utf-8)

(when (eq system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  ;; (when (fboundp 'set-fontset-font)
  ;;   (set-fontset-font "fontset-default"
  ;;                     '(#x1F600 . #x1F64F)
  ;;                     (font-spec :name "Apple Color Emoji") nil 'prepend))
  )

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

;;
;; add hooks
;;

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; Show trailing whitespace
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eshell-mode-hook
                eww-mode
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(add-hook 'comint-mode-hook (lambda ()
                              ;; (toggle-truncate-lines 1)
                              (make-local-variable 'jit-lock-defer-timer)
                              (set (make-local-variable 'jit-lock-defer-time) 0.25)))

;;
;; key binds
;;

;; unbind unused keys
(global-unset-key "\C-z") ; don’t suspend on C-z
(global-unset-key [?\s-p]) ; printing crashes occasionally
(global-unset-key (kbd "C-x C-e"))
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c C-u") 'rename-uniquely)
(global-set-key (kbd "C-x ~") (lambda () (interactive) (find-file "~")))
(global-set-key (kbd "C-x /") (lambda () (interactive) (find-file "/")))
(global-set-key (kbd "C-c l") 'browse-url-at-point)
(global-set-key (kbd "C-x 5 3") 'iconify-frame)
(global-set-key (kbd "C-x v f") 'vc-git-grep)
(global-set-key (kbd "s-SPC") 'cycle-spacing)
(global-set-key (kbd "C-c v") 'customize-variable)

(apply #'hook-into-modes (lambda ()
                           (define-key (current-local-map) (kbd "C-x C-e")
                             'eval-last-sexp)
                           ) lisp-mode-hooks)

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
  :config
  (setq-default abbrev-mode t))

(use-package ace-window
  :bind ("M-o" . ace-window))

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
  :preface
  ;; Compilation from Emacs
  (defun colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))
    )

  :init (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package apropospriate-theme
  :demand
  :config
  (load-theme 'apropospriate-dark t))

(use-package browse-at-remote
  :commands browse-at-remote)

(use-package buffer-move
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package bury-successful-compilation
  :commands bury-successful-compilation
  :init
  (bury-successful-compilation 1))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.c\\'"                   . c-mode)
         ("\\.cpp\\'"                 . c++-mode)
         ("\\.c++\\'"                 . c++-mode)
         ("\\.mm\\'"                  . c++-mode))
  :preface
  (defun my-paste-as-check ()
    (interactive)
    (save-excursion
      (insert "/*\n")
      (let ((beg (point)) end)
        (yank)
        (setq end (point-marker))
        (goto-char beg)
        (while (< (point) end)
          (forward-char 2)
          (insert "CHECK: ")
          (forward-line 1)))
      (insert "*/\n")))

  (defvar printf-index 0)

  (defun insert-counting-printf (arg)
    (interactive "P")
    (if arg
        (setq printf-index 0))
    (if t
        (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                        (setq printf-index (1+ printf-index))))
      (insert (format "printf(\"step %d..\\n\");\n"
                      (setq printf-index (1+ printf-index)))))
    (forward-line -1)
    (indent-according-to-mode)
    (forward-line))

  (defun my-c-mode-common-hook ()
    ;; (ggtags-mode 1)
    (hs-minor-mode 1)
    (hide-ifdef-mode 1)
    ;; (whitespace-mode 1)
    (which-function-mode 1)
    (company-mode 1)
    (bug-reference-prog-mode 1)

    (bind-key "<return>" #'newline-and-indent c-mode-base-map)

    (unbind-key "M-j" c-mode-base-map)
    (bind-key "C-c C-i" #'c-includes-current-file c-mode-base-map)
    (bind-key "C-c C-y" #'my-paste-as-check c-mode-base-map)

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indicate-empty-lines t)
    (setq fill-column 72)

    (bind-key "M-q" #'c-fill-paragraph c-mode-base-map)

    (let ((bufname (buffer-file-name)))
      (when bufname
        (cond
         ((string-match "/ledger/" bufname)
          (c-set-style "ledger"))
         ((string-match "/edg/" bufname)
          (c-set-style "edg"))
         (t
          (c-set-style "clang")))))

    (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|DEBUG\\)("
                                         1 font-lock-warning-face t))))

  :config

  (use-package c-eldoc
    :commands c-turn-on-eldoc-mode
    :init (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

  (bind-key "#" #'self-insert-command c-mode-base-map)
  (bind-key "{" #'self-insert-command c-mode-base-map)
  (bind-key "}" #'self-insert-command c-mode-base-map)
  (bind-key "/" #'self-insert-command c-mode-base-map)
  (bind-key "*" #'self-insert-command c-mode-base-map)
  (bind-key ";" #'self-insert-command c-mode-base-map)
  (bind-key "," #'self-insert-command c-mode-base-map)
  (bind-key ":" #'self-insert-command c-mode-base-map)
  (bind-key "(" #'self-insert-command c-mode-base-map)
  (bind-key ")" #'self-insert-command c-mode-base-map)
  (bind-key "<" #'self-insert-command c++-mode-map)
  (bind-key ">" #'self-insert-command c++-mode-map)

  (add-to-list 'c-style-alist
               '("edg"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . +)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "")))

  (add-to-list 'c-style-alist
               '("ledger"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . 0)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "")))

  (add-to-list 'c-style-alist
               '("clang"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . 0)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . ""))))

(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)))

(use-package company
  :demand
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

(use-package css-mode
  :mode "\\.css\\'"
  :commands css-mode
  :config
  (use-package rainbow-mode
    :commands rainbow-mode
    :init
    (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
      (add-hook hook 'rainbow-mode)))
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
  :bind (("C-c J" . dired-double-jump))

  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  :config
  (defun dired-create-file (filename)
    "Create FILENAME from Dired in if not exists.
If FILENAME already exists do nothing."
    (interactive "FCreate file: ")
    (shell-command (format "touch %s" filename))
    (when (file-exists-p filename)
      (dired-add-file filename)
      (dired-move-to-filename)))
  (define-key dired-mode-map "|" 'dired-create-file)

  (define-key dired-mode-map (kbd "C-c C-c") 'compile)

  (bind-key "r" #'browse-url-of-dired-file dired-mode-map)

  (bind-key "l" #'dired-up-directory dired-mode-map)

  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig))))
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
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
  :init (dtrt-indent-mode 1))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (dumb-jump-mode))

(use-package edebug
  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
            "\\(defun\\|defmacro\\)\\s-+"
            "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen))))
  )

(use-package eldoc
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
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
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (make-local-variable 'eldoc-documentation-function)
              (setq eldoc-documentation-function 'esh-help-eldoc-command)
              (eldoc-mode))))

(use-package eshell
  :bind (("C-c s" . eshell-new))
  :commands (eshell eshell-command)
  :preface

  (defun eshell-new (&optional arg)
    (interactive)
    (setq-local eshell-buffer-name (concat "*eshell<" (expand-file-name default-directory) ">*"))
    (eshell arg))

  (defun eshell/emacs (&rest args)
    "Open a file in Emacs.  Some habits die hard.
ARGS unused"
    (if (null args)
        ;; If I just ran "emacs", I probably expect to be launching
        ;; Emacs, which is rather silly since I'm already in Emacs.
        ;; So just pretend to do what I ask.
        (bury-buffer)
      ;; We have to expand the file names or else weird stuff happens
      ;; when you try to open a bunch of different files in wildly
      ;; different places in the filesystem.
      (mapc #'find-file (mapcar #'expand-file-name args))))

  (defalias 'eshell/emacsclient 'eshell/emacs)

  (defun eshell/vi (&rest args)
    "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer.
ARGS unused"
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (find-file file)
            (forward-line line))
        (find-file (pop args)))))

  (defun eshell/clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (eshell-send-input))

  ;; (defun eshell/ssh (&rest args)
  ;;   (interactive)
  ;;   (insert (apply #'format "cd /scpx:%s:" args))
  ;;   (eshell-send-input))

  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  :config
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (setenv "EDITOR" (concat "emacsclient -c -s " server-name))))

  (defun eshell-cd (&optional dir)
    "Open each directory in a new buffer like dired.
DIR to open if none provided assume HOME dir."
    (interactive)
    (let* ((dir (if dir dir "~"))
           (dir_ (expand-file-name
                  (concat dir "/") default-directory))
           (default-directory (if (string= dir_ "//") "/" dir_))
           )

      (if (file-directory-p default-directory)
          (let (
                (buffer
                 (get-buffer-create (concat "*eshell<"
                                            default-directory ">*"))))
            (pop-to-buffer-same-window buffer)
            (unless (derived-mode-p 'eshell-mode)
              (eshell-mode))
            )
        (error "Not a directory")
        )
      ))

  (add-hook 'eshell-mode-hook (lambda () (defalias 'eshell/cd 'eshell-cd) ))

  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient -nq")

  ;; (defadvice eshell (before dotemacs activate)
  ;;   (setq eshell-banner-message (concat (shell-command-to-string "@fortune@/bin/fortune") "\n")))

  ;;  (defun eshell-ls-find-file-at-point (point)
  ;;    "RET on Eshell's `ls' output to open files."
  ;;    (interactive "d")
  ;;    (find-file (buffer-substring-no-properties
  ;;                (previous-single-property-change point 'help-echo)
  ;;                (next-single-property-change point 'help-echo))))

  ;;  (defun eshell-ls-find-file-at-mouse-click (event)
  ;;    "Middle click on Eshell's `ls' output to open files.
  ;; From Patrick Anderson via the wiki."
  ;;    (interactive "e")
  ;;    (eshell-ls-find-file-at-point (posn-point (event-end event))))

  ;;  (defvar eshell-ls-keymap--clickable
  ;;    (let ((map (make-sparse-keymap)))
  ;;      (define-key map (kbd "<return>") 'eshell-ls-find-file-at-point)
  ;;      (define-key map (kbd "<mouse-1>") 'eshell-ls-find-file-at-mouse-click)
  ;;      (define-key map (kbd "<mouse-2>") 'eshell-ls-find-file-at-mouse-click)
  ;;      map))

  ;;  (defun eshell-ls-decorated-name--clickable (file)
  ;;    "Eshell's `ls' now lets you click or RET on file names to open them."
  ;;    (add-text-properties 0 (length (car file))
  ;;                         (list 'help-echo "RET, mouse-2: visit this file"
  ;;                               'mouse-face 'highlight
  ;;                               'keymap eshell-ls-keymap--clickable)
  ;;                         (car file))
  ;;    file)

  ;;  (advice-add 'eshell-ls-decorated-name :after #'eshell-ls-decorated-name--clickable)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map [(control ?u)] nil)))
  )

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :init
  (setq eshell-prompt-function 'epe-theme-lambda))

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
  :disabled
  :commands flycheck-irony-setup
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package ghc)

(use-package gist
  :bind ("C-c C-g" . gist-region-or-buffer-private)
  :commands (gist-list gist-region gist-region-private gist-buffer
                       gist-buffer-private gist-region-or-buffer gist-region-or-buffer-private))

(use-package gnus
  :commands gnus
  :bind (("C-M-g" . gnus) ("C-x n u" . gnus))
  :init
  (add-hook 'kill-emacs-hook (lambda ()
                               (when (boundp 'gnus-group-exit)
				 (gnus-group-exit))))
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package go-eldoc
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :mode "\\.go\\'")

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep))
  :config
  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49)))

(use-package gud
  :commands gud-gdb
  :bind ("C-. g" . show-debugger)
  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
      (if gud-buf
          (switch-to-buffer-other-window gud-buf)
        (call-interactively 'gud-gdb))))
  :config
  (bind-key "<f9>" #'gud-cont)
  (bind-key "<f10>" #'gud-next)
  (bind-key "<f11>" #'gud-step)
  (bind-key "S-<f11>" #'gud-finish)
  )

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package hideshow
  :commands (hs-minor-mode)
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package hideshowvis
  :disabled
  :commands (hideshowvis-minor-mode hideshowvis-symbols)
  :init
  ;; (add-hook 'prog-mode-hook 'hideshowvis-minor-mode)
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

(use-package ivy
  :demand
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
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
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind (("C-x k" . kill-or-bury-alive)
         ("C-c r" . kill-or-bury-alive-purge-buffers)))

(use-package lisp-mode
  :preface
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (defvar slime-mode nil)
  (defvar lisp-mode-initialized nil)

  (defun my-lisp-mode-hook ()
    (use-package edebug
      :demand)

    (use-package eldoc
      :commands eldoc-mode
      :demand
      )

    (use-package elint
      :commands 'elint-initialize
      :preface
      (defun elint-current-buffer ()
        (interactive)
        (elint-initialize)
        (elint-current-buffer))

      :config
      (add-to-list 'elint-standard-variables 'current-prefix-arg)
      (add-to-list 'elint-standard-variables 'command-line-args-left)
      (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
      (add-to-list 'elint-standard-variables 'emacs-major-version)
      (add-to-list 'elint-standard-variables 'window-system))

    (defun my-elisp-indent-or-complete (&optional arg)
      (interactive "p")
      (call-interactively 'lisp-indent-line)
      (unless (or (looking-back "^\\s-*")
                  (bolp)
                  (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
        (call-interactively 'lisp-complete-symbol)))

    (defun my-lisp-indent-or-complete (&optional arg)
      (interactive "p")
      (if (or (looking-back "^\\s-*") (bolp))
          (call-interactively 'lisp-indent-line)
        (call-interactively 'slime-indent-and-complete-symbol)))

    (defun my-byte-recompile-file ()
      (save-excursion
        (byte-recompile-file buffer-file-name)))

    (auto-fill-mode 1)
    (bind-key "<tab>" #'my-elisp-indent-or-complete emacs-lisp-mode-map)

    (add-hook 'after-save-hook 'check-parens nil t)

    (unless (memq major-mode
                  '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
      (bind-key "M-q" #'slime-reindent-defun lisp-mode-map)
      (bind-key "M-l" #'slime-selector lisp-mode-map)))

  ;; Change lambda to an actual lambda symbol
  :init
  (mapc
   (lambda (major-mode)
     (font-lock-add-keywords
      major-mode
      '(("(\\(lambda\\)\\>"
         (0 (ignore
             (compose-region (match-beginning 1)
                             (match-end 1) ?λ))))
        ("(\\|)" . 'esk-paren-face)
        ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face
            nil t)))))
   lisp-modes)

  (apply #'hook-into-modes 'my-lisp-mode-hook lisp-mode-hooks)
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
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))

  :bind
  (("<C-S-down>" . mc/mark-next-like-this) ;; broken by macOS
   ("<C-S-up>" . mc/mark-previous-like-this) ;; keybinds
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package neotree
  :commands (neotree-show neotree-hide)
  :preface
  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
                                        ; (projectile-project-root)
             (ffip-project-root)))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  :bind (("<f8>" . neotree-project-dir-toggle))
  )

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package noflet
  :commands noflet
  :preface

  ;; (defadvice org-capture-finalize
  ;;     (after delete-capture-frame activate)
  ;;   "Advise capture-finalize to close the frame"
  ;;   (if (equal "capture" (frame-parameter nil 'name))
  ;;       (delete-frame)))

  ;; (defadvice org-capture-destroy
  ;;     (after delete-capture-frame activate)
  ;;   "Advise capture-destroy to close the frame"
  ;;   (if (equal "capture" (frame-parameter nil 'name))
  ;;       (delete-frame)))

  (defun make-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")))
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
            (org-capture)))
  )

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
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package page-break-lines
  :commands page-break-lines-mode
  :init
  (add-hook 'doc-mode-hook 'page-break-lines-mode)
  (add-hook 'help-mode-hook 'page-break-lines-mode)
  (add-hook 'emacs-lisp-mode-hook 'page-break-lines-mode)
  )

(use-package paren
  :demand
  :config (show-paren-mode +1))

(use-package php-mode
  :mode "\\.php\\'")

(use-package projectile
  :demand
  :bind-keymap ("C-c p" . projectile-command-map)
  :config

  (put 'projectile-project-compilation-cmd 'safe-local-variable
       (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command))
                                   compilation-read-command))))

  (projectile-global-mode)

  (use-package easymenu
    :demand
    :config
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
  :interpreter ("python" . python-mode)
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook 'my-python-mode-hook)
  )

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (apply #'hook-into-modes 'rainbow-delimiters-mode lisp-mode-hooks))

(use-package realgud
  :commands (realgud:jdb))

(use-package restart-emacs
  :commands restart-emacs)

(use-package rg
  :commands rg)

(use-package rtags
  :commands (rtags-start-process-unless-running rtags-enable-standard-keybindings)
  :init

  (defun is-current-file-tramp ()
    "Is the current file in a tramp remote setup?"
    (tramp-tramp-file-p (buffer-file-name (current-buffer))))

  ;; Start rtags upon entering a C/C++ file
  (add-hook
   'c-mode-common-hook
   (lambda () (if (not (is-current-file-tramp))
             (rtags-start-process-unless-running))))
  (add-hook
   'c++-mode-common-hook
   (lambda () (if (not (is-current-file-tramp))
             (rtags-start-process-unless-running))))

  :config
  ;; Keybindings
  (rtags-enable-standard-keybindings c-mode-base-map "\C-cr")
  )

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :config
  (defun my-ruby-smart-return ()
    (interactive)
    (when (memq (char-after) '(?\| ?\" ?\'))
      (forward-char))
    (call-interactively 'newline-and-indent))

  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys)
    (bind-key "<return>" #'my-ruby-smart-return ruby-mode-map)
    (bind-key "C-h C-i" #'helm-yari ruby-mode-map))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
  )

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package savehist
  :demand
  :config
  (savehist-mode +1))

(use-package saveplace
  :demand
  :config
  ;; activate it for all buffers
  (setq-default save-place t))

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
  :bind (:map shell-mode-map
              ("C-x C-e" . shell-command-at-point))
  )

(use-package shell
  :commands (shell shell-mode)
  :bind ("C-c C-s" . shell)
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  :config
  (setenv "PAGER" "cat")
  (setenv "TERM" "xterm-256color")
  (setenv "EDITOR" "emacsclient -nq"))

(use-package shell-script-mode
  :commands shell-script-mode
  :mode (("\\.zsh\\'" . shell-script-mode)))

(use-package smart-hungry-delete
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
  (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
  :config
  (use-package smartparens-config
    :demand)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (show-smartparens-mode +1)
  )

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

  (defun my-term-hook ()
    (goto-address-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (define-key term-raw-map "\C-y" 'my-term-paste))
  ;; (add-hook 'term-mode-hook 'my-term-hook)
  )

(use-package tern
  :commands tern-mode
  :init
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package toc-org
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package transpose-frame
  :bind ("C-x t" . transpose-frame))

(use-package try
  :commands try)

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)))

(use-package which-key
  :demand
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".clean" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (when (and (file-exists-p file)
                 (not (file-exists-p ".noclean"))
                 (not (and buffer-file-name
                           (string-match "\\.texi\\'" buffer-file-name))))
        (add-hook 'write-contents-hooks
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)
  )

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package windmove
  :demand
  :config (windmove-default-keybindings 'meta))

(use-package xterm-color
  :demand
  :config
  (setenv "TERM" "xterm-256color")
  ;; Comint and Shell
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'compilation-start-hook
            (lambda (proc)
              ;; We need to differentiate between compilation-mode buffers
              ;; and running as part of comint (which at this point we assume
              ;; has been configured separately for xterm-color)
              (when (eq (process-filter proc) 'compilation-filter)
                ;; This is a process associated with a compilation-mode buffer.
                ;; We may call `xterm-color-filter' before its own filter function.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string)))))))
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package notmuch
  :commands notmuch)

(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-e" . crux-eval-and-replace)
         ([shift return] . crux-smart-open-line))
  ;; (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ;; (global-set-key (kbd "C-c o") #'crux-open-with)
  ;; (global-set-key (kbd "s-r") #'crux-recentf-find-file)
  ;; (global-set-key (kbd "C-<backspace>" #'crux-kill-line-backwards))
  ;; (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  )

(use-package sudo-edit
  :bind (("C-c C-r" . sudo-edit)))

(provide 'default)
;;; default.el ends here
