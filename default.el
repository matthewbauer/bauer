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

(use-package apropospriate-theme
  :demand
  :config (load-theme 'apropospriate-dark t))

(use-package server
  :disabled
  :builtin
  :demand
  ;; :if (and (boundp 'server-process) (not server-process))
  :config
  (server-start)
  )

(use-package tramp
  :builtin
  :demand)

(setenv "NIX_SSL_CERT_FILE" "/etc/ssl/certs/ca-bundle.crt")
(setenv "EDITOR" "emacsclient -nw")
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "PAGER" "cat")

(defun epe-abbrev-dir-name (dir)
  "Return the base directory name of DIR."
  )

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
 '(dired-recursive-copies 'top)
 '(dired-recursive-deletes 'top)
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
 '(savehist-additional-variables '(search-ring regexp-search-ring))
 '(savehist-autosave-interval 60)
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
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-ensure-function 'ignore)
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(use-package-verbose t)
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
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(when (eq system-type 'darwin)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-~") 'ns-prev-frame)
  )

(add-hook 'prog-mode-hook 'bug-reference-prog-mode)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; (add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'server-switch-hook 'raise-frame)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;
;; key binds
;;

;; unbind unused keys
(global-unset-key [?\s-p]) ; printing crashes occasionally
(global-set-key (kbd "C-c C-k") 'eval-buffer)
(global-set-key (kbd "C-c C-u") 'rename-uniquely)
(global-set-key (kbd "C-x ~") (lambda () (interactive) (find-file "~")))
(global-set-key (kbd "C-x /") (lambda () (interactive) (find-file "/")))
(global-set-key (kbd "C-c l") 'browse-url-at-point)
(global-set-key (kbd "C-x 5 3") 'iconify-frame)
(global-set-key (kbd "C-x 5 4") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-x v f") 'vc-git-grep)
(global-set-key (kbd "s-SPC") 'cycle-spacing)
;; (global-set-key (kbd "C-c v") 'customize-variable)

(bind-key* "<C-return>" #'other-window)
(bind-key "C-z" #'delete-other-windows)
(bind-key "M-g l" #'goto-line)
(bind-key "<C-M-backspace>" #'backward-kill-sexp)
;; (bind-key "C-x D" #'edit-rectangle)
;; (bind-key "C-x d" #'delete-whitespace-rectangle)
;; (bind-key "C-x F" #'set-fill-column)
(bind-key "C-x t" #'toggle-truncate-lines)
(bind-key "C-x v H" #'vc-region-history)
;; (bind-key "C-c <tab>" #'ff-find-other-file)
(bind-key "C-c SPC" #'just-one-space)
(bind-key "C-c f" #'flush-lines)
;; (bind-key "C-c k" #'keep-lines)
(bind-key "C-c o" #'customize-option)
(bind-key "C-c O" #'customize-group)
(bind-key "C-c F" #'customize-face)
(bind-key "C-c q" #'fill-region)
;; (bind-key "C-c r" #'replace-regexp)
(bind-key "C-c s" #'replace-string)
(bind-key "C-c u" #'rename-uniquely)
(bind-key "C-c v" #'ffap)
(bind-key "C-c z" #'clean-buffer-list)
(bind-key "C-c =" #'count-matches)
(bind-key "C-c ;" #'comment-or-uncomment-region)

;; buffer switching
(bind-key "s-{" 'previous-buffer)
(bind-key "s-}" 'next-buffer)

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

(global-unset-key (kbd "C-x C-e"))
(create-hook-helper always-eval-sexp ()
  :hooks (lisp-mode-hook emacs-lisp-mode-hook)
  (define-key (current-local-map) (kbd "C-x C-e") 'pp-eval-last-sexp))

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

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

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
  :disabled
  :builtin
  :config (setq-default abbrev-mode t))

(use-package ace-window
  :bind (("M-o" . other-window)
         ("C-x o" . ace-window)))

(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp)
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
          (when (eq major-mode 'compilation-mode)
            (let ((inhibit-read-only t))
              (ansi-color-apply-on-region (point-min) (point-max))))))

(use-package anything
  :commands anything)

(use-package autorevert
  :builtin
  :commands global-auto-revert-mode
  :demand
  :config (global-auto-revert-mode t))

(use-package bash-completion
  :disabled
  :commands bash-completion-dynamic-complete
  :init (add-hook 'shell-dynamic-complete-functions
                  'bash-completion-dynamic-complete))

(use-package bm)

(use-package browse-at-remote
  :commands browse-at-remote)

(use-package buffer-move
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package bug-reference-github
  :commands bug-reference-github-set-url-format
  :init (add-hook 'prog-mode-hook 'bug-reference-github-set-url-format))

(use-package bury-successful-compilation
  :disabled
  :commands bury-successful-compilation
  :init (bury-successful-compilation 1))

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
  :preface
  (defun my-complete ()
    (interactive)
    (cond ((eq major-mode 'jdee-mode) (jdee-complete-menu)
           (t (company-complete-common-or-cycle)))))

  :bind (("<C-tab>" . my-complete)
         ("M-C-/" . company-complete)
         :map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous))
  :diminish company-mode
  :commands (company-mode global-company-mode company-complete-common)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))
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
  :disabled
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

(use-package delsel
  :builtin
  :demand
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

(use-package dired-collapse
  :after dired
  :disabled
  :commands dired-collapse-mode
  :init (add-hook 'dired-mode-hook 'dired-collapse-mode))

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

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :demand
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
  :init (add-hook 'prog-mode-hook 'editorconfig-mode))

(use-package eldoc
  :builtin
  :commands eldoc-mode
  :init (add-hooks '(((emacs-lisp-mode
                       eval-expression-minibuffer-setup) . eldoc-mode)))
  :config
  (defalias 'eldoc-get-fnsym-args-string 'elisp-get-fnsym-args-string)
  )

(use-package elec-pair ;; should disable in sp modes
  :builtin
  :commands electric-pair-mode
  :init (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package electric ;; should disable in sp modes
  :builtin
  :commands (electric-quote-mode electric-indent-mode)
  :init
  (add-hook 'prog-mode-hook 'electric-quote-mode)
  (add-hook 'prog-mode-hook 'electric-indent-mode)
  )

(use-package electric-operator
  :disabled
  :commands electric-operator-mode
  :init (add-hook 'prog-mode-hook 'electric-operator-mode))

(use-package elfeed
  :commands elfeed)

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode))

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
  :commands (eshell eshell-command))

(use-package eshell-extras
  :commands eshell-extras-setup
  :after eshell
  :config (eshell-extras-setup)
  ;; in local dir
  :builtin
  )

(use-package eshell-prompt-extras
  :disabled
  :commands epe-theme-lambda
  :init (setq eshell-prompt-function 'epe-theme-lambda))

(use-package ess-site
  :name "ess"
  :commands R)

(use-package esup
  :commands esup)

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (setq eval-expr-print-function 'pp
        eval-expr-print-level 20
        eval-expr-print-length 100)
  )

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package ffap
  :disabled
  :builtin
  :config
  (defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
    (if (string= ad-return-value "/")
        (setq ad-return-value nil)))

  (defvar ffap-file-at-point-line-number nil
    "Variable to hold line number from the last `ffap-file-at-point' call.")

  (defadvice ffap-file-at-point (after ffap-store-line-number activate)
    "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
    (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
           (name
            (or (condition-case nil
                    (and (not (string-match "//" string)) ; foo.com://bar
                         (substitute-in-file-name string))
                  (error nil))
                string))
           (line-number-string
            (and (string-match ":[0-9]+" name)
                 (substring name (1+ (match-beginning 0)) (match-end 0))))
           (line-number
            (and line-number-string
                 (string-to-number line-number-string))))
      (if (and line-number (> line-number 0))
          (setq ffap-file-at-point-line-number line-number)
        (setq ffap-file-at-point-line-number nil))))

  (defadvice find-file-at-point (after ffap-goto-line-number activate)
    "If `ffap-file-at-point-line-number' is non-nil goto this line."
    (when ffap-file-at-point-line-number
      (with-no-warnings
        (goto-line ffap-file-at-point-line-number))
      (setq ffap-file-at-point-line-number nil)))
  )

(use-package fill-column-indicator
  :disabled
  :commands fci-mode
  :init (add-hook 'prog-mode-hook 'fci-mode))

(use-package firestarter
  :disabled
  :commands firestarter-mode
  :init (firestarter-mode))

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

(use-package ggtags
  :builtin
  )

(use-package ghc)

(use-package gist
  :disabled
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

(use-package hookify
  :commands hookify)

(use-package hydra
  :bind (("C-x t" . hydra-toggle/body)
         ("<f5>" . hydra-zoom/body)
         ("C-M-g" . hydra-error/body))
  :config
  (hydra-add-font-lock)

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
  )

(use-package corral
  :disabled
  :bind ("C-c v" . hydra-corral/body)
  :config
  (setq corral-preserve-point t)

  (defhydra hydra-corral (:columns 4)
    "Corral"
    ("(" corral-parentheses-backward "Back")
    (")" corral-parentheses-forward "Forward")
    ("[" corral-brackets-backward "Back")
    ("]" corral-brackets-forward "Forward")
    ("{" corral-braces-backward "Back")
    ("}" corral-braces-forward "Forward")
    ("." hydra-repeat "Repeat"))
  )

(use-package ibuffer
  :builtin
  :bind ("C-x b" . ibuffer))

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

(use-package indent-shift
  :disabled
  :bind (("C-c <" . indent-shift-left)
         ("C-c >" . indent-shift-right)))

(use-package indium
  :mode ("\\.js\\'" . indium-mode)
  :commands (indium-mode indium-interaction-mode indium-scratch))

(use-package info
  :builtin
  :bind ("C-h C-i" . info-lookup-symbol)
  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
        (delete-window))
    ))

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
  :after projectile
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq dumb-jump-selector 'ivy)
  (setq rtags-display-result-backend 'ivy)
  :config (ivy-mode 1))

(use-package jdee
  :mode ("\\.java\\'" . jdee-mode)
  :commands jdee-mode
  :bind (:map jdee-mode-map
              ("<s-mouse-1>" . jdee-open-class-at-event)))

(use-package jka-compr
  :builtin
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

(use-package json-mode)

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
         :keymap magit-mode-map
         ("C-o" . magit-dired-other-window)))

(use-package magithub
  :disabled
  :after magit
  :config (magithub-feature-autoinject t))

(use-package make-it-so
  :disabled
  :demand
  :config (mis-config-default))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :commands markdown-mode)

(use-package minimap
  :commands minimap-mode)

(use-package mmm-mode
  :commands mmm-mode
  :config
  (use-package mmm-auto
    :builtin
    :demand))

(use-package mode-line-debug
  :disabled
  :commands mode-line-debug-mode
  :init (mode-line-debug-mode))

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

(use-package neotree
  :disabled
  :bind (("<f8>" . neotree-toggle)))

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
  (use-package ox-pandoc
    :disabled
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

(use-package paren
  :builtin
  :demand
  :disabled
  :config (show-paren-mode 1))

(use-package php-mode
  :mode "\\.php\\'")

(use-package popwin
  ;; :bind-keymap ("C-z" . popwin:keymap)
  :commands popwin-mode
  :init (popwin-mode 1)
  :config
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '(term-mode :position :top :height 16 :stick t) popwin:special-display-config)
  )

(use-package prog-mode
  :builtin
  :commands (prettify-symbols-mode global-prettify-symbols-mode)
  :init
  (add-hook 'prog-mode-hook 'prettify-symbols-mode)
  ;; (global-prettify-symbols-mode)
  )

(use-package projectile
  ;; :bind ("s-f" . hydra-projectile/body)
  :bind-keymap ("C-c p" . projectile-command-map)

  :config

  ;; (defhydra hydra-projectile (:color blue :columns 4)
  ;;   "Projectile"
  ;;   ("a" counsel-git-grep "ag")
  ;;   ("b" projectile-switch-to-buffer "switch to buffer")
  ;;   ("c" projectile-compile-project "compile project")
  ;;   ("d" projectile-find-dir "dir")
  ;;   ("f" projectile-find-file "file")
  ;;   ;; ("ff" projectile-find-file-dwim "file dwim")
  ;;   ;; ("fd" projectile-find-file-in-directory "file curr dir")
  ;;   ("g" ggtags-update-tags "update gtags")
  ;;   ("i" projectile-ibuffer "Ibuffer")
  ;;   ("K" projectile-kill-buffers "Kill all buffers")
  ;;   ;; ("o" projectile-multi-occur "multi-occur")
  ;;   ("p" projectile-switch-project "switch")
  ;;   ("r" projectile-run-async-shell-command-in-root "run shell command")
  ;;   ("x" projectile-remove-known-project "remove known")
  ;;   ("X" projectile-cleanup-known-projects "cleanup non-existing")
  ;;   ("z" projectile-cache-current-file "cache current")
  ;;   ("q" nil "cancel")
  ;;   )

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
        ))))

(use-package proof-site
  :name "proofgeneral"
  :demand)

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
  :builtin
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package savehist
  :builtin
  :demand
  :commands savehist-mode
  :config (savehist-mode 1))

(use-package saveplace
  :disabled
  :builtin
  :commands save-place-mode
  :demand
  :config (save-place-mode t))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package semantic
  :builtin
  :disabled
  )

(use-package sentence-navigation
  :bind (("M-e" . sentence-nav-forward)
         ("M-a" . sentence-nav-backward)))

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
  :commands smart-hungry-delete-add-default-hooks
  :bind (:map prog-mode-map
              ("<backspace>" . smart-hungry-delete-backward-char)
              ("C-d" . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

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
  :commands (smartparens-mode show-smartparens-mode)
  :init
  (setq sp-base-key-bindings 'paredit)
  (add-hooks '(((emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 ielm-mode
                 lisp-mode
                 inferior-lisp-mode
                 lisp-interaction-mode
                 slime-repl-mode
                 eval-expression-minibuffer-setup) . smartparens-mode)))
  (add-hooks '(((emacs-lisp-mode
                 inferior-emacs-lisp-mode
                 ielm-mode
                 lisp-mode
                 inferior-lisp-mode
                 lisp-interaction-mode
                 slime-repl-mode) . show-smartparens-mode)))
  :config
  (use-package smartparens-config
    :builtin
    :demand)
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
  :bind ("C-c t" . my-term)
  :config
  (defadvice ansi-term (before force-bash)
    (interactive (list explicit-shell-file-name)))
  (defadvice term (before force-bash)
    (interactive (list explicit-shell-file-name)))
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
	(let ((buffer (process-buffer proc)))
	  ad-do-it
	  (kill-buffer buffer))
      ad-do-it))
  )

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
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  )

(use-package time
  :builtin
  :commands display-time
  :config (display-time))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package try
  :commands try)

(use-package typescript-mode
  :mode "\\.ts\\'")

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
  :demand
  :commands which-function-mode
  :config (which-function-mode t))

(use-package which-key
  :commands which-key-mode
  :demand
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package windmove
  ;; :bind (("C-M-o" . hydra-window/body))
  :builtin
  ;; :demand
  :commands windmove-default-keybindings
  :init
  (windmove-default-keybindings 'meta)

  ;;   :config
  ;;   (defhydra hydra-window (:color amaranth)
  ;;     "
  ;; Move Point^^^^   Move Splitter   ^Ace^                       ^Split^
  ;; --------------------------------------------------------------------------------
  ;; _w_, _<up>_      Shift + Move    _C-a_: ace-window           _2_: split-window-below
  ;; _a_, _<left>_                    _C-s_: ace-window-swap      _3_: split-window-right
  ;; _s_, _<down>_                    _C-d_: ace-window-delete    ^ ^
  ;; _d_, _<right>_                   ^   ^                       ^ ^
  ;; You can use arrow-keys or WASD.
  ;; "
  ;;     ("2" split-window-below nil)
  ;;     ("3" split-window-right nil)
  ;;     ("a" windmove-left nil)
  ;;     ("s" windmove-down nil)
  ;;     ("w" windmove-up nil)
  ;;     ("d" windmove-right nil)
  ;;     ("A" hydra-move-splitter-left nil)
  ;;     ("S" hydra-move-splitter-down nil)
  ;;     ("W" hydra-move-splitter-up nil)
  ;;     ("D" hydra-move-splitter-right nil)
  ;;     ("<left>" windmove-left nil)
  ;;     ("<down>" windmove-down nil)
  ;;     ("<up>" windmove-up nil)
  ;;     ("<right>" windmove-right nil)
  ;;     ("<S-left>" hydra-move-splitter-left nil)
  ;;     ("<S-down>" hydra-move-splitter-down nil)
  ;;     ("<S-up>" hydra-move-splitter-up nil)
  ;;     ("<S-right>" hydra-move-splitter-right nil)
  ;;     ("C-a" ace-window nil)
  ;;     ("u" hydra--universal-argument nil)
  ;;     ("C-s" (lambda () (interactive) (ace-window 4)) nil)
  ;;     ("C-d" (lambda () (interactive) (ace-window 16)) nil)
  ;;     ("q" nil "quit"))
  )

(use-package with-editor
  :disabled
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command))
  :commands with-editor-export-editor
  :init (add-hooks '(((shell-mode
                       term-exec
                       eshell-mode) . with-editor-export-editor)))
  )

(use-package with-editor
  :commands (with-editor-async-shell-command
	     with-editor-shell-command))

(use-package xterm-color
  :demand
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

(use-package yasnippet
  :disabled
  :commands yas-minor-mode
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

(use-package ycmd)

(use-package elf-mode
  :commands elf-mode
  :init (add-to-list 'magic-mode-alist (cons "ELF" 'elf-mode)))

(use-package winner-mode
  :disabled)

(use-package transpose-frame
  :bind ("H-t" . transpose-frame))

(use-package dirtrack
  :disabled
  :builtin
  :commands dirtack-mode
  :init (add-hook 'comint-mode-hook 'dirtrack-mode))

(provide 'default)
;;; default.el ends here
