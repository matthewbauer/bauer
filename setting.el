;; Settings.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action (quote accept))
 '(apropos-do-all t)
 '(auth-source-save-behavior t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify t)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms (\` ((".*" (\, temporary-file-directory) t))))
 '(backup-directory-alist (\` ((".*" \, temporary-file-directory))))
 '(backward-delete-char-untabify-method (quote hungry))
 '(bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
 '(byte-compile-verbose nil)
 '(c-eldoc-includes "" t)
 '(company-auto-complete nil)
 '(company-continue-commands
   (quote
    (not save-buffer save-some-buffers save-buffers-kill-terminal save-buffers-kill-emacs comint-previous-matching-input-from-input comint-next-matching-input-from-input)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-occurrence-weight-function (quote company-occurrence-prefer-any-closest))
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(company-tooltip-limit 10)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
 '(completions-format (quote vertical))
 '(counsel-find-file-at-point t)
 '(counsel-mode-override-describe-bindings t)
 '(cperl-clobber-lisp-bindings t)
 '(cperl-continued-statement-offset 8)
 '(cperl-electric-keywords nil)
 '(cperl-electric-lbrace-space t)
 '(cperl-electric-linefeed nil)
 '(cperl-electric-parens nil)
 '(cperl-font-lock t)
 '(cperl-indent-level 4)
 '(cperl-info-on-command-no-prompt t)
 '(cperl-invalid-face nil)
 '(cperl-lazy-help-time 3)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(default-input-method "TeX")
 '(dired-dwim-target t)
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
 '(dumb-jump-quiet t)
 '(dumb-jump-selector (quote ivy))
 '(editorconfig-exclude-modes
   (quote
    (gnus-group-mode gnus-summary-mode gnus-article-mode message-mode fundamental-mode erc-mode dired-mode)))
 '(enable-recursive-minibuffers t)
 '(erc-autoaway-idle-seconds 600)
 '(erc-autoaway-use-emacs-idle t)
 '(erc-autojoin-timing (quote ident))
 '(erc-fill-column 88)
 '(erc-fill-prefix "          ")
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track)))
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display (quote buffer))
 '(erc-server "irc.freenode.net")
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
 '(eshell-cmpl-expand-before-complete t)
 '(eshell-cmpl-ignore-case t)
 '(eshell-complete-export-definition t)
 '(eshell-cp-interactive-query t)
 '(eshell-cp-overwrite-files nil)
 '(eshell-default-target-is-dot t)
 '(eshell-destroy-buffer-when-process-dies t)
 '(eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
 '(eshell-highlight-prompt nil)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 10000)
 '(eshell-list-files-after-cd t)
 '(eshell-ln-interactive-query t)
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-term eshell-tramp eshell-unix eshell-xtra)))
 '(eshell-mv-interactive-query t)
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt eshell-truncate-buffer)))
 '(eshell-plain-echo-behavior nil)
 '(eshell-prompt-function (quote epe-theme-lambda))
 '(eshell-review-quick-commands t)
 '(eshell-rm-interactive-query t)
 '(eshell-send-direct-to-subprocesses nil)
 '(eshell-visual-commands
   (quote
    ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "nano")))
 '(eval-expression-debug-on-error t)
 '(explicit-bash-args (quote ("-c" "export EMACS=; stty echo; bash")))
 '(explicit-shell-file-name "bash")
 '(fased-completing-read-function (quote nil))
 '(fill-column 80)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-global-modes
   (quote
    (not erc-mode message-mode git-commit-mode view-mode outline-mode text-mode)))
 '(flycheck-idle-change-delay 0.8)
 '(flycheck-standard-error-navigation t)
 '(flyspell-abbrev-p nil)
 '(flyspell-auto-correct nil)
 '(flyspell-highlight-properties nil)
 '(flyspell-incorrect-hook nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format
   (quote
    ((:eval
      (if
          (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b")))) t)
 '(gc-cons-threshold 100000000)
 '(golden-ratio-exclude-modes
   (quote
    ("ediff-mode" "gud-mode" "gdb-locals-mode" "gdb-registers-mode" "gdb-breakpoints-mode" "gdb-threads-mode" "gdb-frames-mode" "gdb-inferior-io-mode" "gud-mode" "gdb-inferior-io-mode" "gdb-disassembly-mode" "gdb-memory-mode" "magit-log-mode" "magit-reflog-mode" "magit-status-mode" "IELM" "dired-mode" "neotree-mode")))
 '(golden-ratio-extra-commands
   (quote
    (windmove-left windmove-right windmove-down windmove-up ace-window)))
 '(golden-ratio-recenter nil)
 '(history-delete-duplicates t)
 '(history-length 20000)
 '(iedit-toggle-key-default nil)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "")
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-program-name "aspell")
 '(ispell-quietly t)
 '(ivy-count-format "\"\"")
 '(ivy-display-style nil)
 '(ivy-minibuffer-faces nil)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(load-prefer-newer t)
 '(mac-command-key-is-meta t)
 '(mac-command-modifier 'meta)
 '(mac-option-key-is-meta nil)
 '(mac-frame-tabbing t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-clone-set-remote\.pushDefault t)
 '(magit-completing-read-function (quote magit-builtin-completing-read))
 '(magit-delete-by-moving-to-trash t)
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-mode-hook
   (quote
    (magit-load-config-extensions magit-xref-setup bug-reference-mode turn-on-magit-gh-pulls)))
 '(magit-no-confirm t)
 '(magit-process-connection-type nil)
 '(magit-process-find-password-functions (quote (magit-process-password-auth-source)))
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-revision-mode-hook (quote (bug-reference-mode)))
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:
  ")))
 '(magit-save-repository-buffers (quote dontask))
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(make-backup-files nil)
 '(network-security-level (quote medium))
 '(next-error-recenter (quote (4)))
 '(nrepl-log-messages t)
 '(nsm-save-host-names t)
 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 '(parse-sexp-ignore-comments t)
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching t)
 '(projectile-enable-idle-timer t)
 '(projectile-idle-timer-hook nil)
 '(projectile-ignored-project-function (quote file-remote-p))
 '(projectile-mode-line
   (quote
    (:eval
     (if
         (projectile-project-p)
         (format " P[%s]"
                 (projectile-project-name))
       ""))))
 '(projectile-switch-project-action (quote projectile-dired))
 '(projectile-verbose nil)
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(restart-emacs-restore-frames nil)
 '(ring-bell-function (quote ignore))
 '(rtags-completions-enabled t)
 '(rtags-display-result-backend (quote ivy))
 '(rtags-imenu-syntax-highlighting 10)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote access-label)
           (quote -))
     (eval c-set-offset
           (quote substatement-open)
           0)
     (eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote +))
     (eval c-set-offset
           (quote arglist-cont)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote +))
     (eval c-set-offset
           (quote inline-open)
           0)
     (eval c-set-offset
           (quote defun-open)
           0)
     (eval c-set-offset
           (quote innamespace)
           0)
     (indicate-empty-lines . t))))
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-place-file (concat user-emacs-directory "places"))
 '(savehist-additional-variables (quote (search-ring regexp-search-ring)))
 '(savehist-autosave-interval 60)
 '(savehist-file (expand-file-name "savehist" user-emacs-directory))
 '(semanticdb-default-save-directory
   (quote
    (expand-file-name "semanticdb" user-emacs-directory)))
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(sh-learn-basic-offset t)
 '(shell-completion-execonly nil)
 '(shell-input-autoexpand nil)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-base-key-bindings (quote paredit))
 '(sp-hybrid-kill-entire-symbol nil)
 '(temporary-file-directory (expand-file-name "tmp" user-emacs-directory))
 '(term-input-autoexpand t)
 '(term-input-ring-file-name t)
 '(tls-checktrust t)
 '(undo-limit 800000)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(use-package-always-defer t)
 '(use-package-always-ensure nil)
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(use-package-verbose nil)
 '(vc-allow-async-revert t)
 '(vc-command-messages t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(visible-bell nil)
 '(visible-cursor nil)
 '(whitespace-line-column 80)
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'settings)
;;; settings.el ends here
