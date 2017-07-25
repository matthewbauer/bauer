;;; default -- @matthewbauer’s Emacs config

;;; Commentary:

;;; This should be used in conjunction with Nixpkgs in config.

;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'use-package-keywords :builtin)
(defun use-package-handler/:builtin (name keyword arg rest state)
  (use-package-process-keywords name rest state))
(add-to-list 'use-package-keywords :name)
(defun use-package-handler/:name (name keyword arg rest state)
  (use-package-process-keywords name rest state))

(setq use-package-always-defer t)
(setq use-package-always-ensure nil)
(setq use-package-expand-minimally t)
(setq use-package-verbose nil)

(use-package apropospriate-theme
  :init
  (let ((filename (locate-library "apropospriate-theme")))
    (when filename
      (add-to-list 'custom-theme-load-path (file-name-directory filename)))))

(use-package server
  :disabled
  :builtin
  :commands server-start
  :init
  (add-hook 'after-init-hook 'server-start t)
  ;; (add-hook 'server-switch-hook 'raise-frame)
  )

(use-package tramp
  :builtin
  :commands (tramp-tramp-file-p
             tramp-file-name-user
             tramp-file-name-real-host
             tramp-dissect-file-name))

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
      (put symbol 'default-value (list value))
      ;; (put symbol 'saved-value (list value))
      (put symbol 'standard-value (list value))
      ;; (put symbol 'force-value t)
      (funcall set symbol (eval value)))))

(set-defaults
 '(ad-redefinition-action 'accept)
 '(ag-executable "@ag@/bin/ag")
 '(apropos-do-all t)
 '(async-shell-command-buffer 'new-buffer)
 '(auth-source-save-behavior t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-verbose nil)
 '(auto-save-visited-file-name t)
 '(backward-delete-char-untabify-method 'hungry)
 '(backup-by-copying t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(bm-buffer-persistence t)
 '(bm-restore-repository-on-load t)
 '(bm-cycle-all-buffers t)
 '(c-syntactic-indentation nil)
 '(column-number-mode t)
 '(comint-scroll-show-maximum-output nil)
 '(company-auto-complete nil)
 '(company-continue-commands
   '(not save-buffer
         save-some-buffers
         save-buffers-kill-terminal
         save-buffers-kill-emacs
         comint-previous-matching-input-from-input
         comint-next-matching-input-from-input))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-occurrence-weight-function 'company-occurrence-prefer-any-closest)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-limit 10)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-environment '("TERM=xterm-256color"))
 '(compilation-scroll-output nil)
 '(compilation-skip-threshold 2)
 '(completions-format 'vertical)
 '(completion-cycle-threshold 5)
 '(counsel-find-file-at-point t)
 '(counsel-mode-override-describe-bindings t)
 '(create-lockfiles nil)
 '(custom-buffer-done-kill t)
 '(custom-search-field nil)
 '(create-lockfiles nil)
 '(debug-on-signal t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-dwim-target t)
 '(dired-omit-verbose nil)
 '(dired-omit-files "^\\.")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-subtree-line-prefix " ")
 '(dtrt-indent-verbosity 0)
 '(display-time-default-load-average nil)
 '(display-time-format "")
 '(display-buffer-alist
   (\`(((\,(rx bos (or "*Flycheck errors*"
                       "*Backtrace"
                       "*Warnings"
                       "*compilation"
                       "*Help"
                       "*less-css-compilation"
                       "*Packages"
                       "*magit-process"
                       "*SQL"
                       "*tldr")))
        (display-buffer-reuse-window display-buffer-in-side-window)
        (side . bottom)
        (reusable-frames . visible)
        (window-height . 0.33))
       ("." nil
        (reusable-frames . visible)))))
 '(display-buffer-reuse-frames t)
 '(dumb-jump-quiet t)
 '(enable-recursive-minibuffers nil)
 '(epg-gpg-program "@gpg@/bin/gpg")
 '(epg-gpgconf-program "@gpg@/bin/gpgconf")
 '(epg-gpgsm-program "@gpg@/bin/gpgsm")
 '(erc-autoaway-idle-seconds 600)
 '(erc-autoaway-use-emacs-idle t)
 '(erc-autojoin-timing 'ident)
 '(erc-fill-prefix "          ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-interpret-mirc-color t)
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display 'buffer)
 '(erc-server-coding-system '(utf-8 . utf-8))
 '(erc-timestamp-format "%H:%M ")
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-try-new-nick-p nil)
 '(eshell-banner-message "")
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
 '(eshell-highlight-prompt t)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 10000)
 '(eshell-list-files-after-cd t)
 '(eshell-ln-interactive-query t)
 '(eshell-mv-interactive-query t)
 '(eshell-output-filter-functions '(eshell-handle-ansi-color
                                    eshell-handle-control-codes
                                    eshell-watch-for-password-prompt
                                    eshell-truncate-buffer))
 '(eshell-plain-echo-behavior nil)
 '(eshell-review-quick-commands t)
 '(eshell-rm-interactive-query t)
 '(eshell-prompt-function
   (lambda () (concat
          (when (tramp-tramp-file-p default-directory)
            (concat
             (tramp-file-name-user (tramp-dissect-file-name default-directory))
             "@"
             (tramp-file-name-real-host (tramp-dissect-file-name
                                         default-directory))
             " "))
          (let ((dir (eshell/pwd)))
            (if (string= dir (getenv "HOME")) "~"
              (let ((dirname (file-name-nondirectory dir)))
                (if (string= dirname "") "/" dirname))))
          (if (= (user-uid) 0) " # " " $ "))))
 '(eshell-visual-commands
   '("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm"
     "nano" "nethack" "telnet" "emacs" "emacsclient" "htop" "w3m" "links" "lynx"
     "elinks" "irrsi" "mutt" "finch" "newsbeuter" "pianobar"))
 '(eval-expression-debug-on-error t)
 '(explicit-bash-args '("-c" "export EMACS= INSIDE_EMACS=; stty echo; bash"))
 '(explicit-shell-file-name "bash")
 '(expand-region-contract-fast-key "j")
 '(fased-completing-read-function 'nil)
 '(fill-column 80)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-display-errors-function
   'flycheck-display-error-messages-unless-error-list)
 '(flycheck-standard-error-navigation nil)
 '(flycheck-global-modes '(not erc-mode
                               message-mode
                               git-commit-mode
                               view-mode
                               outline-mode
                               text-mode
                               org-mode))
 '(flyspell-abbrev-p nil)
 '(flyspell-auto-correct nil)
 '(flyspell-highlight-properties nil)
 '(flyspell-incorrect-hook nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil)
 '(fortune-dir "@fortune@/share/games/fortunes")
 '(fortune-file "@fortune@/share/games/fortunes/food")
 '(frame-title-format '(:eval
                        (if (buffer-file-name)
                            (abbreviate-file-name (buffer-file-name))
                          "%b")))
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-non-file-buffers t)
 '(highlight-nonselected-windows nil)
 '(hideshowvis-ignore-same-line nil)
 '(history-delete-duplicates t)
 '(history-length 20000)
 '(iedit-toggle-key-default nil)
 '(imenu-auto-rescan t)
 '(imap-ssl-program '("@gnutls@/bin/gnutls-cli --tofu -p %p %s"))
 '(indicate-empty-lines t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(inhibit-startup-echo-area-message t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message "")
 '(ispell-extra-args '("--sug-mode=ultra"))
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
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-no-confirm t)
 '(magit-process-connection-type nil)
 '(magit-process-find-password-functions '(magit-process-password-auth-source))
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-save-repository-buffers 'dontask)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(make-backup-files nil)
 '(minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
 '(mmm-global-mode 'buffers-with-submode-classes)
 '(mmm-submode-decoration-level 2)
 '(mwim-beginning-of-line-function 'beginning-of-line)
 '(mwim-end-of-line-function 'end-of-line)
 '(neo-theme 'arrow)
 '(neo-fixed-size nil)
 '(next-error-recenter '(4))
 '(notmuch-show-logo nil)
 '(nrepl-log-messages t)
 '(nsm-save-host-names t)
 '(ns-function-modifier 'hyper)
 '(ns-pop-up-frames nil)
 '(org-support-shift-select t)
 '(parens-require-spaces t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")))
 '(package-enable-at-startup nil)
 '(parse-sexp-ignore-comments t)
 '(proof-splash-enable nil)
 '(projectile-globally-ignored-files '(".DS_Store" "TAGS"))
 '(projectile-enable-caching t)
 '(projectile-enable-idle-timer nil)
 '(projectile-mode-line
   '(:eval (if (and (projectile-project-p)
                    (not (file-remote-p default-directory)))
               (format " Projectile[%s]" (projectile-project-name)) "")))
 '(projectile-ignored-project-function 'file-remote-p)
 '(projectile-switch-project-action 'projectile-dired)
 '(projectile-do-log nil)
 '(projectile-verbose nil)
 '(reb-re-syntax 'string)
 '(require-final-newline t)
 '(resize-mini-windows t)
 '(ring-bell-function 'ignore)
 '(ripgrep-executable "@ripgrep@/bin/rg")
 '(rtags-path "@rtags@/bin")
 '(rtags-completions-enabled t)
 '(rtags-imenu-syntax-highlighting 10)
 '(ruby-insert-encoding-magic-comment nil)
 '(same-window-buffer-names
   '("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*"))
 '(save-abbrevs 'silently)
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
 '(savehist-autosave-interval 60)
 '(scroll-preserve-screen-position 'always)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space nil)
 '(shell-completion-execonly nil)
 '(shell-input-autoexpand nil)
 '(sp-autoskip-closing-pair 'always)
 '(sp-hybrid-kill-entire-symbol nil)
 '(truncate-lines nil)
 '(tab-always-indent 'complete)
 '(term-input-autoexpand t)
 '(term-input-ignoredups t)
 '(term-input-ring-file-name t)
 '(tramp-default-proxies-alist '(((regexp-quote (system-name)) nil nil)
                                 (nil "\\`root\\'" "/ssh:%h:")
                                 (".*" "\\`root\\'" "/ssh:%h:")))
 '(tramp-remote-path '(tramp-own-remote-path
                       "/run/current-system/sw/bin"
                       tramp-default-remote-path
                       "/bin"
                       "/usr/bin"
                       "/sbin"
                       "/usr/sbin"
                       "/usr/local/bin"
                       "/usr/local/sbin"
                       "/opt/bin"
                       "/opt/sbin"
                       "~/.nix-profile/bin"
                       ))
 '(text-quoting-style 'quote)
 '(tls-checktrust t)
 '(tls-program "@gnutls@/bin/gnutls-cli --tofu -p %p %h")
 '(undo-limit 800000)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'forward)
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(use-package-ensure-function 'ignore)
 '(use-package-enable-imenu-support t)
 '(version-control t)
 '(vc-allow-async-revert t)
 '(vc-command-messages nil)
 '(vc-git-diff-switches '("-w" "-U3"))
 '(vc-ignore-dir-regexp
   "\\(\\(\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\)\\|\\(\\`/[^/|:][^/|]*:\\)\\)\\|\\(\\`/[^/|:][^/|]*:\\)")
 '(view-read-only t)
 '(view-inhibit-help-message t)
 '(visible-bell nil)
 '(visible-cursor nil)
 '(woman-imenu t)
 '(whitespace-line-column 80)
 '(whitespace-auto-cleanup t)
 '(whitespace-rescan-timer-time nil)
 '(whitespace-silent t)
 '(whitespace-style '(face
                      trailing
                      lines
                      space-before-tab
                      empty
                      lines-style))
 )

(use-package hook-helpers
  :commands (create-hook-helper define-hook-helper))

(use-package add-hooks
  :commands (add-hooks add-hooks-pair))

;; (require 'f)

(setq enable-recursive-minibuffers t)

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten y or n confirm

;; (prefer-coding-system 'utf-8)

;; (when (fboundp 'recentf-mode)
;;   (recentf-mode -1))
;; (when (fboundp 'shell-dirtrack-mode)
;;   (shell-dirtrack-mode -1))

;; these should be disabled before Emsacs displays the frame
;; but we disable them here just in case
(when (and (fboundp 'menu-bar-mode)
           (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'time-stamp)

;; (global-visual-line-mode t)

;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)

;;
;; key binds
;;

(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c C-u") 'rename-uniquely)
(global-set-key (kbd "C-x ~") (lambda () (interactive) (find-file "~")))
(global-set-key (kbd "C-x /") (lambda () (interactive) (find-file "/")))
(global-set-key (kbd "C-c l") 'browse-url-at-point)
(global-set-key (kbd "C-x 5 3") 'iconify-frame)
(global-set-key (kbd "C-x 5 4") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x v f") 'vc-git-grep)
(global-set-key (kbd "s-SPC") 'cycle-spacing)
(global-set-key (kbd "C-c w w") 'whitespace-mode)

(global-set-key (kbd "C-x 8 : )") "☺")

(bind-key* "<C-return>" 'other-window)
(bind-key "C-z" 'delete-other-windows)
(bind-key "M-g l" 'goto-line)
(bind-key "<C-M-backspace>" 'backward-kill-sexp)
(bind-key "C-x t" 'toggle-truncate-lines)
(bind-key "C-x v H" 'vc-region-history)
(bind-key "C-c SPC" 'just-one-space)
(bind-key "C-c f" 'flush-lines)
(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)
(bind-key "C-c F" 'customize-face)
(bind-key "C-c q" 'fill-region)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)
(bind-key "C-c v" 'ffap)
(bind-key "C-c z" 'clean-buffer-list)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)

;; Compiling
(bind-key "H-c" 'compile)
(bind-key "H-r" 'recompile)
(bind-key "H-s" (defun save-and-recompile ()
                  (interactive)
                  (save-buffer)
                  (recompile)))
(bind-key "s-1" 'other-frame)
(bind-key "<s-return>" 'toggle-frame-fullscreen)

(bind-key "s-C-<left>"  'shrink-window-horizontally)
(bind-key "s-C-<right>" 'enlarge-window-horizontally)
(bind-key "s-C-<down>"  'shrink-window)
(bind-key "s-C-<up>"    'enlarge-window)

(when (eq system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-~") 'ns-prev-frame)
  ;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
  ;;                   (selected-frame) 'prepend)
  )

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

(advice-add async-shell-command :before
            (lambda ()
              (let ((buf (get-buffer "*Async Shell Command*")))
                (if buf (let ((proc (get-buffer-process buf)))
                          (if (and proc (eq 'run (process-status proc)))
                              (with-current-buffer buf
                                (rename-uniquely))))))))

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

(use-package ace-window
  :bind (("M-o" . other-window)
         ("C-x o" . ace-window)))

(use-package ag
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp)
  :bind ("C-?" . ag-project))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init (add-hooks '(((emacs-lisp-mode
                       inferior-emacs-lisp-mode
                       ielm-mode
                       lisp-mode
                       inferior-lisp-mode
                       lisp-interaction-mode
                       slime-repl-mode) . aggressive-indent-mode))))

(use-package align
  :bind (("C-c [" . align-regexp))
  :commands align
  :builtin
  )

(use-package ansi-color
  :builtin
  :init (create-hook-helper colorize-compilation-buffer ()
          :hooks (compilation-filter-hook)
          (let ((inhibit-read-only t))
            (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package anything
  :commands anything)

(use-package autorevert
  :builtin
  :defer 1
  :config
  (global-auto-revert-mode t)
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package bash-completion
  :disabled
  :commands bash-completion-dynamic-complete
  :init (add-hook 'shell-dynamic-complete-functions
                  'bash-completion-dynamic-complete))

(use-package bm)

(use-package bool-flip
  :bind ("C-c C-b" . bool-flip-do-flip))

(use-package browse-at-remote
  :commands browse-at-remote)

(use-package buffer-move
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package bug-reference
  :builtin
  :commands bug-reference-prog-mode
  :init
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode)
  )

(use-package bug-reference-github
  :commands bug-reference-github-set-url-format
  :init (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format))

(use-package cc-mode
  :builtin
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.c\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.c++\\'" . c++-mode)
         ("\\.mm\\'" . c++-mode))
  :config
  (use-package c-eldoc
    :commands c-turn-on-eldoc-mode
    :init (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))
  )

(use-package cmake-ide
  :disabled
  :commands cmake-ide-setup
  :init (cmake-ide-setup))

(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)))

(use-package company
  :diminish company-mode
  :bind (("<C-tab>" . company-complete)
         ("M-C-/" . company-complete)
         :map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous))
  :commands (company-mode global-company-mode company-complete-common)
  :init (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-irony
  :commands company-irony
  :after company
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-tern
  :after company
  :commands company-tern
  :config (add-to-list 'company-backends 'company-tern))

(use-package company-web
  :after company
  :commands company-web
  :config (add-to-list 'company-backends 'company-web))

(use-package compile
  :builtin
  :bind (("C-c C-c" . compile)
         ("M-O" . show-compilation))
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

(use-package copy-as-format
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)))

(use-package counsel
  :commands (counsel-descbinds)
  :bind* (([remap execute-extended-command] . counsel-M-x)
          ("C-c d" . counsel-dired-jump)
          ("<f1> f" . counsel-describe-function)
          ("<f1> v" . counsel-describe-variable)
          ("C-x C-f" . counsel-find-file)
          ("<f1> l" . counsel-find-library)
          ;; ("C-c g" . counsel-git)
          ("C-c j" . counsel-git-grep)
          ("C-c k" . counsel-ag)
          ("C-x l" . counsel-locate)
          ("C-M-i" . counsel-imenu)
          ("M-y" . counsel-yank-pop)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-toggle 1))

(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-e" . crux-eval-and-replace)
         ([shift return] . crux-smart-open-line)))

(use-package css-mode
  :builtin
  :mode "\\.css\\'"
  :commands css-mode
  :config
  (use-package css-eldoc
    :demand)
  )

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package delsel
  :builtin
  :defer 2
  :config (delete-selection-mode t))

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
  :builtin
  :bind (("C-c J" . dired-double-jump)
         :map dired-mode-map
         ("C-c C-c" . compile)
         ("r" . browse-url-of-dired-file)
         ("M-!" . async-shell-command)
	 ("e" . eshell))
  :init
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  )

(use-package dired-collapse
  :after dired
  :disabled
  :commands dired-collapse-mode
  :init (add-hook 'dired-mode-hook 'dired-collapse-mode))

(use-package dired-column
  :builtin
  :after dired
  :bind (:map dired-mode-map
              ("o" . dired-column-find-file)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	      ("<tab>" . dired-subtree-toggle)
	      ("<backtab>" . dired-subtree-cycle)))

(use-package dired-x
  :builtin
  :after dired
  :commands dired-omit-mode
  :init
  ;; toggle `dired-omit-mode' with C-x M-o
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  )

(use-package direx
  :bind (("C-x C-j" . direx:jump-to-directory))
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
	popwin:special-display-config))

(use-package drag-stuff
  :bind ("C-c d" . drag-stuff-mode))

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

(use-package edebug
  :builtin
  )

(use-package editorconfig
  :commands editorconfig-mode
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook 'editorconfig-mode))

(use-package eldoc
  :builtin
  :commands eldoc-mode
  :init (add-hooks '(((emacs-lisp-mode
                       eval-expression-minibuffer-setup
                       lisp-mode-interactive-mode
                       typescript-mode) . eldoc-mode)))
  :config
  (defalias 'eldoc-get-fnsym-args-string 'elisp-get-fnsym-args-string)
  )

(use-package elec-pair ;; should disable in sp modes
  :builtin
  :commands electric-pair-mode
  :init (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package electric ;; should disable in sp modes
  :builtin
  :commands (electric-quote-mode electric-indent-mode electric-layout-mode)
  :init
  (add-hook 'prog-mode-hook 'electric-quote-mode)
  (add-hook 'prog-mode-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook 'electric-layout-mode)
  )

(use-package elf-mode
  :commands elf-mode
  ;; TODO: use :magic
  :init (add-to-list 'magic-mode-alist (cons "ELF" 'elf-mode)))

(use-package elfeed
  :commands elfeed)

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode))

(use-package em-dired
  :commands em-dired-mode
  :init (add-hook 'eshell-mode-hook 'em-dired-mode)
  ;; in local dir
  :builtin
  )

(use-package emacs-lisp-mode
  :builtin
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package erc
  :builtin
  :bind ("C-x r c" . erc))

(use-package esh-help
  :commands esh-help-eldoc-command
  :init (create-hook-helper esh-help-setup ()
          :hooks (eshell-mode-hook)
          (make-local-variable 'eldoc-documentation-function)
          (setq eldoc-documentation-function 'esh-help-eldoc-command)
          (eldoc-mode)))

(use-package eshell
  :builtin
  :bind (("C-c M-t" . eshell)
	 ("C-c x" . eshell))
  :commands (eshell eshell-command)
  :init
  (setq eshell-modules-list
        '(eshell-alias
          eshell-banner
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-rebind
          eshell-script
          eshell-smart
          eshell-term
          eshell-tramp
          eshell-unix
          eshell-xtra))
  )

(use-package ess-site
  :name "ess"
  :commands R)

(use-package esup
  :commands esup)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package firestarter
  :bind ("C-c m s" . firestarter-mode))

(use-package flycheck
  :commands global-flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :commands flycheck-irony-setup
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flyspell
  :builtin
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package focus
  :bind ("C-c m f" . focus-mode))

(use-package ggtags
  :builtin
  )

(use-package ghc)

(use-package gist
  :bind ("C-c C-g" . gist-region-or-buffer-private)
  :commands (gist-list gist-region gist-region-private gist-buffer
                       gist-buffer-private gist-region-or-buffer
                       gist-region-or-buffer-private))

(use-package gnus
  :builtin
  :commands gnus
  :init
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package go-eldoc
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :mode "\\.go\\'")

(use-package god-mode
  :bind (("<escape>" . god-local-mode)))

(use-package goto-addr
  :builtin
  :commands goto-address-prog-mode
  :init
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  )

(use-package grep
  :builtin
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep)))

(use-package gud
  :builtin
  :commands gud-gdb
  :bind (("C-. g" . show-debugger)
         ("<f9>" . gud-cont)
         ("<f10>" . gud-next)
         ("<f11>" . gud-step)
         ("S-<f11>" . gud-finish)))

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)))

(use-package hideshow
  :builtin
  :commands hs-minor-mode
  :init (add-hooks '(((c-mode-common
                       lisp-mode
                       emacs-lisp-mode
                       java-mode) . hs-minor-mode))))

(use-package hideshowvis
  :disabled
  :commands (hideshowvis-minor-mode hideshowvis-symbols)
  :init
  (add-hook 'prog-mode-hook 'hideshowvis-minor-mode)
  )

(use-package hippie-exp
  :builtin
  :bind (("M-/" . hippie-expand))
  :preface
  (defun my-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
   The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (flet ((ding))        ; avoid the (ding) when hippie-expand exhausts its
                                        ; options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'my-hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defun my-try-expand-company (old)
    (require 'company)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     company-candidates))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
        (he-init-string (he-tag-beg) (point))
        (setq he-expand-list
              (sort (all-completions he-search-string 'tags-complete-tag)
                    'string-lessp)))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (when old (he-reset-string))
            ())
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

  (defun my-dabbrev-substring-search (pattern &optional reverse limit)
    (let ((result ())
          (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
                         (concat (regexp-quote pattern) "\\sw+"))
                        ((eq (char-syntax (aref pattern 0)) ?_)
                         (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
                        (t
                         (concat (regexp-quote pattern)
                                 "\\(\\sw\\|\\s_\\)+")))))
      (while (and (not result)
                  (if reverse
                      (re-search-backward regpat limit t)
                    (re-search-forward regpat limit t)))
        (setq result (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (skip-syntax-backward "w_")
                        (point))
                      (match-end 0)))
        (if (he-string-member result he-tried-table t)
            (setq result nil)))     ; ignore if bad prefix or already in table
      result))

  (defun try-my-dabbrev-substring (old)
    (let ((old-fun (symbol-function 'he-dabbrev-search)))
      (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
      (unwind-protect
          (try-expand-dabbrev old)
        (fset 'he-dabbrev-search old-fun))))

  (defun try-expand-flexible-abbrev (old)
    "Try to complete word using flexible matching.
  Flexible matching works by taking the search string and then
  interspersing it with a regexp for any character. So, if you try
  to do a flexible match for `foo' it will match the word
  `findOtherOtter' but also `fixTheBoringOrange' and
  `ifthisisboringstopreadingnow'.
  The argument OLD has to be nil the first call of this function, and t
  for subsequent calls (for further possible completions of the same
  string).  It returns t if a new completion is found, nil otherwise."
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     (he-flexible-abbrev-collect he-search-string)))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-flexible-abbrev-collect (str)
    "Find and collect all words that flex-matches STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (let ((collection nil)
          (regexp (he-flexible-abbrev-create-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          ;; Is there a better or quicker way than using `thing-at-point'
          ;; here?
          (setq collection (cons (thing-at-point 'word) collection))))
      collection))

  (defun he-flexible-abbrev-create-regexp (str)
    "Generate regexp for flexible matching of STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
            "\\w*" "\\b"))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :config
  (setq hippie-expand-try-functions-list
        '(my-try-expand-company
	  try-my-dabbrev-substring
	  my-try-expand-dabbrev-visible
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-expand-tag
	  try-expand-flexible-abbrev
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-expand-line-all-buffers
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))
  )

(use-package hl-todo
  ;; TODO: add font-lock highlighting for @nethack@ substitutions
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package hookify
  :commands hookify)

(use-package hydra
  :bind (("C-x t" . hydra-toggle/body)
         ("<f5>" . hydra-zoom/body)
         ("C-M-g" . hydra-error/body)
         ("C-c h c" . hydra-case/body)
         ("C-c h z" . hydra-zoom/body)
         ("C-c h e" . hydra-error/body)
         ("C-c h p" . hydra-projectile/body)
         ("C-c h w" . hydra-window/body))
  :disabled
  :config (hydra-add-font-lock)
  :preface
  (eval-and-compile
    (defhydra hydra-error (:color amaranth)
      "goto-error"
      ("h" flycheck-list-errors "first")
      ("j" flycheck-next-error "next")
      ("k" flycheck-previous-error "prev")
      ("v" recenter-top-bottom "recenter")
      ("q" nil "quit"))

    (defhydra hydra-zoom (:color blue :hint nil)
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))

    (defhydra hydra-case ()
      "case"
      ("c" string-inflection-all-cycle nil)
      ("c" string-inflection- nil)
      )

    (defhydra hydra-projectile (:color blue :columns 4)
      "Projectile"
      ("a" counsel-git-grep "ag")
      ("b" projectile-switch-to-buffer "switch to buffer")
      ("c" projectile-compile-project "compile project")
      ("d" projectile-find-dir "dir")
      ("f" projectile-find-file "file")
      ;; ("ff" projectile-find-file-dwim "file dwim")
      ;; ("fd" projectile-find-file-in-directory "file curr dir")
      ("g" ggtags-update-tags "update gtags")
      ("i" projectile-ibuffer "Ibuffer")
      ("K" projectile-kill-buffers "Kill all buffers")
      ;; ("o" projectile-multi-occur "multi-occur")
      ("p" projectile-switch-project "switch")
      ("r" projectile-run-async-shell-command-in-root "run shell command")
      ("x" projectile-remove-known-project "remove known")
      ("X" projectile-cleanup-known-projects "cleanup non-existing")
      ("z" projectile-cache-current-file "cache current")
      ("q" nil "cancel")
      )

    (defhydra hydra-window (:color amaranth)
      "
  Move Point^^^^   Move Splitter   ^Ace^                       ^Split^
  --------------------------------------------------------------------------------
  _w_, _<up>_      Shift + Move    _C-a_: ace-window           _2_: split-window-below
  _a_, _<left>_                    _C-s_: ace-window-swap      _3_: split-window-right
  _s_, _<down>_                    _C-d_: ace-window-delete    ^ ^
  _d_, _<right>_                   ^   ^                       ^ ^
  You can use arrow-keys or WASD.
  "
      ("2" split-window-below nil)
      ("3" split-window-right nil)
      ("a" windmove-left nil)
      ("s" windmove-down nil)
      ("w" windmove-up nil)
      ("d" windmove-right nil)
      ("A" hydra-move-splitter-left nil)
      ("S" hydra-move-splitter-down nil)
      ("W" hydra-move-splitter-up nil)
      ("D" hydra-move-splitter-right nil)
      ("<left>" windmove-left nil)
      ("<down>" windmove-down nil)
      ("<up>" windmove-up nil)
      ("<right>" windmove-right nil)
      ("<S-left>" hydra-move-splitter-left nil)
      ("<S-down>" hydra-move-splitter-down nil)
      ("<S-up>" hydra-move-splitter-up nil)
      ("<S-right>" hydra-move-splitter-right nil)
      ("C-a" ace-window nil)
      ("u" hydra--universal-argument nil)
      ("C-s" (lambda () (interactive) (ace-window 4)) nil)
      ("C-d" (lambda () (interactive) (ace-window 16)) nil)
      ("q" nil "quit"))
    )
  )

(use-package ibuffer
  :builtin
  :bind ("C-x b" . ibuffer))

(use-package idle-highlight-mode
  :disabled
  :commands idle-highlight-mode
  :init (add-hooks '(((java-mode
                       emacs-lisp-mode
                       clojure-lisp-mode) . idle-highlight-mode))))

(use-package iedit
  :builtin
  :bind (("C-;" . iedit-mode)
         :map help-map ("C-;" . iedit-mode-toggle-on-function)
         :map esc-map ("C-;" . iedit-mode-toggle-on-function)
         :map isearch-mode-map ("C-;" . iedit-mode-toggle-on-function))
  )

(use-package ielm
  :builtin
  :bind ("C-c :" . ielm))

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package imenu-list
  :commands imenu-list)

(use-package indium
  :mode ("\\.js\\'" . indium-mode)
  :commands (indium-mode indium-interaction-mode indium-scratch))

(use-package info
  :builtin
  :bind ("C-h C-i" . info-lookup-symbol))

(use-package intero
  :commands intero-mode
  :init (add-hook 'haskell-mode-hook 'intero-mode))

(use-package irony
  :commands irony-mode
  :init (add-hooks '(((c++-mode c-mode objc-mode) . irony-mode))))

(use-package irony-eldoc
  :commands irony-eldoc
  :init (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package ivy
  :diminish ivy-mode
  :after projectile
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :commands ivy-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq dumb-jump-selector 'ivy)
  (setq rtags-display-result-backend 'ivy)
  :config (ivy-mode 1))

(use-package java-mode
  :builtin)

(use-package jdee
  :mode ("\\.java\\'" . jdee-mode)
  :commands jdee-mode
  :bind (:map jdee-mode-map
              ("<s-mouse-1>" . jdee-open-class-at-event)))

(use-package jka-compr
  :builtin
  :disabled
  :demand
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

(use-package js2-mode
  :config
  (js2-imenu-extras-setup))

(use-package js3-mode
  :commands js3-mode)

(use-package json-mode
  :mode "\\.json\\'")

(use-package keyfreq
  :disabled
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind (("C-x k" . kill-or-bury-alive)
         ;; ("C-c r" . kill-or-bury-alive-purge-buffers)
	 ))

(use-package lisp-mode
  :builtin
  )

(use-package llvm-mode
  :mode "\\.ll\\'")

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

(use-package macho-mode
  :commands macho-mode
  :builtin
  ;; TODO: use :magic
  :init
  (add-to-list 'magic-mode-alist '("\xFE\xED\xFA\xCE" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xFE\xED\xFA\xCF" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xCE\xFA\xED\xFE" . macho-mode))
  (add-to-list 'magic-mode-alist '("\xCF\xFA\xED\xFE" . macho-mode))
  )

(use-package magit
  :preface
  (defun magit-dired-other-window ()
    (interactive)
    (dired-other-window (magit-toplevel)))
  :config
  (setq magit-repo-dirs
        (mapcar
         (lambda (dir)
           (substring dir 0 -1))
         (cl-remove-if-not
          (lambda (project)
            (unless (file-remote-p project)
              (file-directory-p (concat project "/.git/"))))
          (projectile-relevant-known-projects))))
  :commands (magit-clone)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup)
         :map magit-mode-map
         ("C-o" . magit-dired-other-window)))

(use-package magithub
  :disabled
  :commands magithub-feature-autoinject
  :init (add-hook 'magit-mode-hook 'magithub-feature-autoinject))

(use-package make-it-so
  :commands mis-mode
  :init (add-hook 'dired-mode-hook 'mis-mode))

(use-package makefile-mode
  :builtin
  :init
  (add-hook 'makefile-mode-hook 'indent-tabs-mode)
  )

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :commands markdown-mode)

(use-package mb-depth
  :builtin
  :commands minibuffer-depth-indicate-mode
  :init (add-hook 'minibuffer-setup-hook 'minibuffer-depth-indicate-mode))

(use-package minimap
  :commands minimap-mode)

(use-package mmm-mode
  :commands mmm-mode
  :config
  (use-package mmm-auto
    :builtin
    :demand))

(use-package move-text
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package multi-term
  :bind (("C-. t" . multi-term-next)
         ("C-. T" . multi-term))
  )

(use-package multiple-cursors
  :bind
  (("<C-S-down>" . mc/mark-next-like-this) ;; broken by macOS shortcut
   ("<C-S-up>" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-!"         . mc/mark-next-symbol-like-this)
   ("s-d"         . mc/mark-all-dwim)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package nix-buffer
  :commands nix-buffer)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package notmuch
  :commands notmuch)

(use-package nroff-mode
  :builtin
  :commands nroff-mode)

(use-package nxml-mode
  :builtin
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  )

(use-package org
  :builtin
  ;; :mode "\\.\\(org\\)\\'"
  :commands org-capture
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c C-l" . org-store-link))
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
    :builtin
    :demand)
  )

(use-package org-bullets
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package pabbrev
  :commands pabbrev-mode)

(use-package page-break-lines
  :commands page-break-lines-mode
  :init (add-hooks '(((doc-mode
                       help-mode
                       emacs-lisp-mode) . page-break-lines-mode))))

(use-package php-mode
  :mode "\\.php\\'")

(use-package popwin
  ;; :bind-keymap ("C-z" . popwin:keymap)
  :defer 2
  :config
  (popwin-mode 1)
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config
        (remove '(compilation-mode :noselect t) popwin:special-display-config))
  (push '("*Help*" :stick t) popwin:special-display-config)
  (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)
  (push '("*dict*" :stick t) popwin:special-display-config)
  (push '("*sdic*" :stick t) popwin:special-display-config)
  (push '(slime-repl-mode :stick t) popwin:special-display-config)
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)
  (push '("*ielm*" :stick t) popwin:special-display-config)
  (push '("*eshell pop*" :stick t) popwin:special-display-config)
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)
  (push '("*haskell*" :stick t) popwin:special-display-config)
  (push '("*GHC Info*") popwin:special-display-config)
  (push '("*git-gutter:diff*" :width 0.5 :stick t)
        popwin:special-display-config)
  (push '("*Occur*" :stick t) popwin:special-display-config)
  (push '("*prodigy*" :stick t) popwin:special-display-config)
  (push '("*Org tags*" :stick t :height 30)
        popwin:special-display-config)
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
  (push '("*ggtags-global*" :stick t :noselect t :height 30)
        popwin:special-display-config)
  (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

  ;; (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '(term-mode :position :top :height 16 :stick t)
        popwin:special-display-config)
  )

(use-package pp
  :builtin
  :commands pp-eval-last-sexp
  :bind (("M-:" . pp-eval-expression))
  :init
  (global-unset-key (kbd "C-x C-e"))
  (create-hook-helper always-eval-sexp ()
    :hooks (lisp-mode-hook emacs-lisp-mode-hook)
    (define-key (current-local-map) (kbd "C-x C-e") 'pp-eval-last-sexp)))

(use-package prog-mode
  :builtin
  :commands (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (add-hook 'prog-mode-hook 'prettify-symbols-mode)
  (create-hook-helper prettify-symbols-prog ()
    ""
    :hooks (prog-mode-hook)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    )
  (create-hook-helper prettify-symbols-lisp ()
    ""
    :hooks (lisp-mode-hook)
    (push '("/=" . ?≠) prettify-symbols-alist)
    )
  (create-hook-helper prettify-symbols-c ()
    ""
    :hooks (c-mode-hook)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist)
    )
  (create-hook-helper prettify-symbols-c++ ()
    ""
    :hooks (c++-mode-hook)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist)
    (push '("->" . ?→) prettify-symbols-alist)
    )
  (create-hook-helper prettify-symbols-js ()
    ""
    :hooks (js2-mode-hook js-mode-hook)
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("=>" . ?⇒) prettify-symbols-alist)
    )
  ;; (global-prettify-symbols-mode)
  )

(use-package projectile
  ;; :bind ("s-f" . hydra-projectile/body)
  :bind-keymap* ("C-c p" . projectile-command-map)

  :config

  (put 'projectile-project-compilation-cmd 'safe-local-variable
       (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command))
                                   compilation-read-command))))

  (projectile-global-mode)

  (use-package easymenu
    :builtin
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
        ["Jump between implementation file and test file"
         projectile-toggle-between-implementation-and-test]
        ["Kill project buffers" projectile-kill-buffers]
        ["Recent files" projectile-recentf]
        ["Edit .dir-locals.el" projectile-edit-dir-locals]
        "--"
        ["Open project in dired" projectile-dired]
        ["Switch to project" projectile-switch-project]
        ["Switch to open project" projectile-switch-open-project]
        ["Discover projects in directory"
         projectile-discover-projects-in-directory]
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
        ))))

(use-package proof-site
  :name "proofgeneral"
  :commands (proofgeneral proof-mode proof-shell-mode))

(use-package python-mode
  :builtin
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hooks '(((emacs-lisp-mode
                       inferior-emacs-lisp-mode
                       ielm-mode
                       lisp-mode
                       inferior-lisp-mode
                       lisp-interaction-mode
                       slime-repl-mode) . rainbow-delimiters-mode))))

(use-package rainbow-mode
  :commands rainbow-mode
  :init (add-hooks '(((emacs-lisp-mode
                       inferior-emacs-lisp-mode
                       ielm-mode
                       lisp-mode
                       inferior-lisp-mode
                       lisp-interaction-mode
                       slime-repl-mode) . rainbow-mode))))

(use-package ranger
  :commands deer)

(use-package readline-complete
  :after company
  :config
  (add-to-list 'company-backends 'company-readline)
  (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1))))

(use-package realgud
  :commands (realgud:jdb))

(use-package restart-emacs
  :commands restart-emacs)

(use-package restclient
  :mode (("\\.rest\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode)))

(use-package rg
  :commands rg)

(use-package rtags
  :commands (rtags-start-process-unless-running
             rtags-enable-standard-keybindings)
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
  :builtin
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package savehist
  :builtin
  :defer 4
  :commands savehist-mode
  :config (savehist-mode 1))

(use-package saveplace
  :builtin
  :commands save-place-mode
  :defer 5
  :config (save-place-mode t))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package sh-script
  :builtin
  :preface
  (defun shell-command-at-point ()
    (interactive)
    (setq start-point (save-excursion
                        (beginning-of-line)
                        (point)))
    (shell-command (buffer-substring start-point (point)))
    )
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :bind (:map sh-mode-map
              ("C-x C-e" . shell-command-at-point))
  )

(use-package shell
  :builtin
  :commands (shell shell-mode)
  :bind ("C-c C-s" . shell)
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )

(use-package skewer-less
  :disabled
  :commands skewer-less-mode
  :init (add-hook 'less-css-mode-hook 'skewer-less-mode))

(use-package slime)

(use-package smart-hungry-delete
  :commands (smart-hungry-delete-default-c-mode-common-hook
             smart-hungry-delete-default-prog-mode-hook
             smart-hungry-delete-default-text-mode-hook)
  :bind (:map prog-mode-map
              ("<backspace>" . smart-hungry-delete-backward-char)
              ("C-d" . smart-hungry-delete-forward-char))
  :init
  (add-hook 'prog-mode-hook 'smart-hungry-delete-default-prog-mode-hook)
  (add-hook 'c-mode-common-hook 'smart-hungry-delete-default-c-mode-common-hook)
  (add-hook 'python-mode-hook 'smart-hungry-delete-default-c-mode-common-hook)
  (add-hook 'text-mode-hook 'smart-hungry-delete-default-text-mode-hook)
  )

(use-package smart-shift
  :bind (("C-c <left>" . smart-shift-left)
	 ("C-c <right>" . smart-shift-right)
	 ("C-c <up>" . smart-shift-up)
	 ("C-c <down>" . smart-shift-down)))

(use-package smart-tabs-mode
  :builtin
  :disabled
  :init (add-hook 'prog-mode-hook 'smart-tabs-mode)
  :commands smart-tabs-mode)

(use-package smartparens
  :commands (smartparens-mode show-smartparens-mode smartparens-strict-mode)
  :bind (:map smartparens-mode-map
              ("C-M-k" . sp-kill-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-s" . sp-splice-sexp)
              ("M-r" . sp-splice-sexp-killing-around)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-<right>" . sp-forward-sexp)
              ("C-M-<left>" . sp-backward-sexp)
              ("M-F" . sp-forward-sexp)
              ("M-B" . sp-backward-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("C-M-e" . sp-up-sexp)
              ("M-r" . sp-unwrap-sexp)
              ("C-(" . sp-forward-barf-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("M-(" . sp-forward-barf-sexp)
              ("M-)" . sp-forward-slurp-sexp)
              ("M-D" . sp-splice-sexp)
              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-splice-sexp-killing-forward)
              ("M-<up>"   . sp-splice-sexp-killing-backward)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)
              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)
              ("C-x C-t" . sp-transpose-hybrid-sexp)
              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c _"  . wrap-with-underscores)
              ("C-c `"  . wrap-with-back-quotes)
              )
  :init
  (add-hooks '(((emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 ielm-mode
                 lisp-mode
                 inferior-lisp-mode
                 lisp-interaction-mode
                 slime-repl-mode
                 eval-expression-minibuffer-setup) . smartparens-strict-mode)))
  (add-hooks '(((emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 ielm-mode
                 lisp-mode
                 inferior-lisp-mode
                 lisp-interaction-mode
                 slime-repl-mode) . show-smartparens-mode)))
  (add-hooks '(((web-mode
                 nxml-mode
                 html-mode) . smartparens-mode)))
  :config
  (use-package smartparens-html
    :builtin
    :demand)
  (use-package smartparens-config
    :builtin
    :demand)

  (bind-key [remap c-electric-backspace]
            'sp-backward-delete-char smartparens-strict-mode-map)
  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (sp-with-modes
      'org-mode
    (sp-local-pair "*" "*"
                   :actions '(insert wrap)
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))

    ;;; Java
  (sp-with-modes
      '(java-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))

  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'clojure-mode "`" "`" :when '(sp-in-string-p))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "~" "~" :actions '(wrap))
  (sp-local-pair 'org-mode "/" "/" :actions '(wrap))
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap))
  )

(use-package string-inflection
  :bind (("C-c r r" . string-inflection-all-cycle)
         ("C-c r c" . string-inflection-camelcase)
         ("C-c r l" . string-inflection-lower-camelcase)
         ;; ("C-c r l" . string-inflection-lisp)
         ("C-c r u" . string-inflection-underscore)
         ("C-c r k" . string-inflection-kebab-case)
         ("C-c r J" . string-inflection-java-style-cycle)))

(use-package subword
  :builtin
  :init (add-hook 'java-mode-hook 'subword-mode))

(use-package sudo-edit
  :bind (("C-c C-r" . sudo-edit)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package term
  :builtin
  :preface
  (defun my-term ()
    (interactive)
    (set-buffer (make-term "my-term" "zsh"))
    (term-mode)
    ;; (term-line-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer "*my-term*"))
  (defun nethack ()
    (interactive)
    (set-buffer (make-term "nethack" "@nethack@/bin/nethack"))
    (term-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer "*nethack*"))
  :bind ("C-c t" . my-term))

(use-package tern
  :commands tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package tex-site
  :name "auctex")

(use-package texinfo
  :mode ("\\.texi\\'" . texinfo-mode))

(use-package text-mode
  :builtin
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  )

(use-package tide
  :commands (tide-setup tide-hl-identifier-mode)
  :init
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  )

(use-package time
  :builtin
  :defer 2
  :config (display-time))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package transpose-frame
  :bind ("H-t" . transpose-frame))

(use-package try
  :commands try)

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package undo-tree
  :disabled
  :init (global-undo-tree-mode 1)
  :bind ("C-c u" . undo-tree-visualize)
  :diminish undo-tree-mode)

(use-package vkill
  :bind ("C-x L" . vkill))

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)))

(use-package which-func
  :builtin
  :defer 6
  :commands which-function-mode
  :config (which-function-mode t))

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :defer 7
  :config (which-key-mode))

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package whitespace-mode
  :builtin
  :commands whitespace-mode
  :init (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package windmove
  :builtin
  :defer 10
  :config (windmove-default-keybindings 'meta))

(use-package with-editor
  :disabled
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command))
  :commands (with-editor-async-shell-command
	     with-editor-shell-command
             with-editor-export-editor)
  :init (add-hooks '(((shell-mode
                       term-exec
                       eshell-mode) . with-editor-export-editor)))
  )

(use-package xterm-color
  :commands xterm-color-filter
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)

  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (define-hook-helper compilation-start (proc)
    (when (eq (process-filter proc) 'compilation-filter)
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string))))))
  )

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package yasnippet
  :disabled
  :commands yas-minor-mode
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

(use-package ycmd)

(use-package shrink-whitespace
  :bind ("H-SPC" . shrink-whitespace))

(provide 'default)
;;; default.el ends here
