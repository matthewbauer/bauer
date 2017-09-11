;;; default -- @matthewbauer’s Emacs config -*- lexical-binding: t -*-

;;; Commentary:

;;; This should be used in conjunction with Nixpkgs in config.

;;; Code:

;; eventually switch to @out@
;; IMPURE: reference to nix-profile!
(defvar output-directory (expand-file-name ".nix-profile" (getenv "HOME")))

(setq
 ;; TODO: restore file-name-handler-alist later on
 ;; file-name-handler-alist nil

 ;; disable gc while initializing
 ;; will be reset to default later on
 gc-cons-threshold 80000000

 ;; ideally this would just use $out
 ;; but we need emacs to be built first
 ;; maybe in the future
 exec-path (append `(,(expand-file-name "bin" output-directory)

                     ;; handle /usr/bin/open and other system-specific stuff
                     "/usr/sbin" "/usr/bin"
                     "/sbin" "/bin"
                     )
                   exec-path)
 ;; TODO: hack browse-url.el to allow customizable open
 )

;; Different from "@out@"

(unless (>= emacs-major-version 25)
  (error "Need Emacs 25+ to work properly"))

;; reset gc
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold
                      (car (get 'gc-cons-threshold 'standard-value)))))

;; garbage collect when window focus is lost
;; (add-hook 'focus-out-hook 'garbage-collect)

(fset 'yes-or-no-p 'y-or-n-p) ;; shorten y or n confirm

;; Work around a bug on OS X where system-name is FQDN.
;; (if (or
;;      (eq system-type 'darwin)
;;      (eq system-type 'berkeley-unix))
;;     (setq system-name (car (split-string (system-name) "\\."))))

(eval-and-compile
  ;; (add-to-list 'load-path "~/.nixpkgs")
  (require 'subr-x)
  (require 'set-defaults)
  (require 'installer)
  )

(defvar man-path `("/usr/share/man"
                   "/usr/local/share/man"
                   ,(expand-file-name "share/man" output-directory)))

(defvar cacert-file "@cacert@/etc/ssl/certs/ca-bundle.crt")

;; setup environment
(set-envs
 `("NIX_SSL_CERT_FILE" ,cacert-file)
 '("NIX_REMOTE" "daemon")
 `("NIX_PATH" ,(concat
                "nixpkgs=/nix/var/nix/profiles/per-user/"
                (getenv "USER")
                "/channels/nixpkgs"))
 '("EDITOR" "emacsclient -nw")
 '("LANG" "en_US.UTF-8")
 '("LC_ALL" "en_US.UTF-8")
 '("PAGER" "cat")
 '("NODE_NO_READLINE" "1")
 `("PATH" ,(string-join exec-path ":"))
 `("MANPATH" ,(string-join man-path ":"))
 )

;; setup defaults
;; should maintain compatiblity with custom.el
(set-defaults
 '(TeX-auto-save t)
 '(TeX-engine 'xetex)
 '(ad-redefinition-action 'accept)
 '(apropos-do-all t)
 '(async-shell-command-buffer 'new-buffer)
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auth-source-save-behavior t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-verbose nil)
 '(auto-save-visited-file-name t)
 '(backward-delete-char-untabify-method 'hungry)
 '(backup-by-copying t)
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
 '(bm-buffer-persistence t)
 '(bm-restore-repository-on-load t)
 '(bm-cycle-all-buffers t)
 '(bookmark-save-flag t)
 '(c-syntactic-indentation nil)
 '(comint-process-echoes t)
 '(comint-input-ignoredups t)
 '(comint-prompt-read-only t)
 '(comint-scroll-show-maximum-output nil)
 '(company-auto-complete (lambda () (and (company-tooltip-visible-p)
                                    (company-explicit-action-p))))
 '(company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
 '(company-continue-commands
   '(not save-buffer
         save-some-buffers
         save-buffers-kill-terminal
         save-buffers-kill-emacs
         comint-previous-matching-input-from-input
         comint-next-matching-input-from-input))
 '(company-require-match nil)
 '(company-selection-wrap-around t)
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
 ;; '(cursor-type 'bar)
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes t)
 '(custom-buffer-done-kill t)
 '(custom-file (expand-file-name "settings.el" user-emacs-directory))
 '(custom-search-field nil)
 '(create-lockfiles nil)
 '(checkdoc-spellcheck-documentation-flag t)
 '(debug-on-signal t)
 '(delete-old-versions t)
 '(delete-by-moving-to-trash t)
 '(dired-auto-revert-buffer t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhv")
 '(dired-omit-verbose nil)
 '(dired-omit-files "^\\.")
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(dired-subtree-line-prefix " ")
 '(dtrt-indent-verbosity 0)
 '(disabled-command-function nil)
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
 '(echo-keystrokes 0)
 '(enable-recursive-minibuffers t)
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
 '(eshell-rebind-keys-alist
   '(([(control 97)] . eshell-bol)
     ([home] . eshell-bol)
     ([(control 100)] . eshell-delchar-or-maybe-eof)
     ([backspace] . eshell-delete-backward-char)
     ([delete] . eshell-delete-backward-char)
     ([(control 119)] . backward-kill-word)
     ([(control 117)] . eshell-kill-input)
     ([tab] . completion-at-point)))
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
 '(eldoc-eval-preferred-function 'pp-eval-expression)
 '(eval-expression-debug-on-error t)
 '(eval-expression-print-length 20)
 '(eval-expression-print-level nil)
 ;; TODO: move to paths?
 '(explicit-bash-args '("-c" "export EMACS= INSIDE_EMACS=; stty echo; bash"))
 '(expand-region-contract-fast-key "j")
 '(fased-completing-read-function 'nil)
 '(fill-column 80)
 '(flycheck-check-syntax-automatically '(save
                                         idle-change
                                         mode-enabled
                                         new-line))
 '(flycheck-display-errors-function
   'flycheck-display-error-messages-unless-error-list)
 '(flycheck-idle-change-delay 0.001)
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
 '(frame-title-format '(:eval
                        (if (buffer-file-name)
                            (abbreviate-file-name (buffer-file-name))
                          "%b")))
 '(global-auto-revert-non-file-buffers t)
 '(highlight-nonselected-windows nil)
 '(hideshowvis-ignore-same-line nil)
 '(history-delete-duplicates t)
 '(history-length 20000)
 '(hippie-expand-verbose nil)
 '(iedit-toggle-key-default nil)
 '(imenu-auto-rescan t)
 '(indicate-empty-lines t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(inhibit-startup-echo-area-message t)
 '(Info-directory-list `(,(expand-file-name "share/info" output-directory)))
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message "")
 '(ispell-extra-args '("--sug-mode=ultra"))
 '(ispell-silently-savep t)
 '(ispell-quietly t)
 '(ivy-count-format "\"\"")
 '(ivy-display-style nil)
 '(ivy-minibuffer-faces nil)
 '(ivy-use-virtual-buffers t)
 '(ivy-fixed-height-minibuffer t)
 ;; '(ivy-re-builders-alist '((swiper . ivy--regex-plus)
 ;;                           (t . ivy--regex-fuzzy)))
 '(jit-lock-defer-time 0.01)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(load-prefer-newer t)
 '(mac-allow-anti-aliasing t)
 '(mac-command-key-is-meta t)
 '(mac-command-modifier 'meta)
 '(mac-option-key-is-meta nil)
 '(mac-option-modifier 'super)
 '(mac-right-option-modifier nil)
 '(mac-frame-tabbing t)
 '(mac-system-move-file-to-trash-use-finder t)
 '(magit-log-auto-more t)
 '(magit-clone-set-remote\.pushDefault t)
 '(magit-diff-options nil)
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
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
 ;; '(mode-line-format nil)
 '(mmm-global-mode 'buffers-with-submode-classes)
 '(mmm-submode-decoration-level 2)
 '(minibuffer-prompt-properties '(read-only t
                                            cursor-intangible t
                                            face minibuffer-prompt))
 '(mwim-beginning-of-line-function 'beginning-of-line)
 '(mwim-end-of-line-function 'end-of-line)
 '(neo-theme 'arrow)
 '(neo-fixed-size nil)
 '(next-error-recenter t)
 '(notmuch-show-logo nil)
 '(nrepl-log-messages t)
 '(nsm-save-host-names t)
 '(ns-function-modifier 'hyper)
 '(ns-pop-up-frames nil)
 '(org-blank-before-new-entry '((heading) (plain-list-item)))
 '(org-return-follows-link t)
 '(org-special-ctrl-a/e t)
 '(org-support-shift-select t)
 '(org-src-fontify-natively t)
 '(parens-require-spaces t)
 '(package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")))
 '(package-enable-at-startup nil)
 '(proof-splash-enable nil)
 '(projectile-globally-ignored-files '(".DS_Store" "TAGS"))
 '(projectile-enable-caching t)
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
 '(rtags-completions-enabled t)
 '(rtags-imenu-syntax-highlighting 10)
 '(ruby-insert-encoding-magic-comment nil)
 '(sh-guess-basic-offset t)
 '(same-window-buffer-names
   '("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*"))
 '(save-abbrevs 'silently)
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables '(search-ring
                                   regexp-search-ring
                                   kill-ring
                                   comint-input-ring))
 '(savehist-autosave-interval 60)
 ;; '(scroll-margin 3)
 ;; '(scroll-conservatively 101)
 ;; '(scroll-up-aggressively 0.01)
 ;; '(scroll-down-aggressively 0.01)
 '(auto-window-vscroll nil)
 '(hscroll-margin 5)
 '(hscroll-step 5)
 '(scroll-preserve-screen-position 'always)
 '(send-mail-function 'smtpmail-send-it)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
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
 ;; TODO: cleanup?
 '(tramp-remote-path `(tramp-own-remote-path
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
                       ,(expand-file-name "bin" output-directory)
                       ))
 '(tramp-default-user nil)
 '(text-quoting-style 'quote)
 '(tls-checktrust t)
 '(undo-limit 800000)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'forward)
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(version-control t)
 '(vc-allow-async-revert t)
 '(vc-command-messages nil)
 '(vc-git-diff-switches '("-w" "-U3"))
 '(vc-follow-symlinks nil)
 '(vc-ignore-dir-regexp
   "\\(\\(\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\)\\|\\(\\`/[^/|:][^/|]*:\\)\\)\\|\\(\\`/[^/|:][^/|]*:\\)")
 '(view-read-only t)
 '(view-inhibit-help-message t)
 '(visible-bell nil)
 '(visible-cursor nil)
 '(woman-imenu t)
 '(woman-manpath man-path)
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

(load custom-file 'noerror)

;; set paths available from Nix substitution
(set-paths
 ;; '(company-clang-executable "@clang@/bin/clang")
 '(company-cmake-executable "@cmake@/bin/cmake")
 '(doc-view-dvipdf-program "@ghostscript@/bin/dvipdf")
 ;; '(doc-view-dvipdfm-program "")
 ;; '(doc-view-pdfdraw-program "")
 ;; '(doc-view-pdftotext-program "")
 ;; '(doc-view-pdfdraw-program "")
 ;; '(doc-view-odf->pdf-converter-program "")
 ;; '(doc-view-pdftotext-program "")
 '(doc-view-ps2pdf-program "@ghostscript@/bin/ps2pdf")
 '(dired-touch-program "@coreutils@/bin/touch")
 '(dired-chmod-program "@coreutils@/bin/chmod")
 '(dired-chown-program "@coreutils@/bin/chown")
 '(dired-free-space-program "@coreutils@/bin/df")
 '(diff-command "@diffutils@/bin/diff")
 '(find-program "@findutils@/bin/find")
 '(epg-gpg-program "@gpg@/bin/gpg")
 '(epg-gpgconf-program "@gpg@/bin/gpgconf")
 '(epg-gpgsm-program "@gpg@/bin/gpgsm")
 ;; '(explicit-shell-file-name (expand-file-name "/bin/bash"))
 '(flycheck-sh-bash-executable "@bash@/bin/bash")
 '(flycheck-sh-zsh-executable "@zsh@/bin/zsh")
 '(flycheck-perl-executable "@perl@/bin/perl")
 ;; '(flycheck-css-csslint-executable "@csslint@/bin/csslint")
 '(flycheck-go-golint-executable "@golint@/bin/golint")
 '(flycheck-python-flake8-executable "@flake8@/bin/flake8")
 '(flycheck-asciidoc-executable "@asciidoc@/bin/asciidoc")
 '(flycheck-less-executable "@lessc@/bin/lessc")
 '(flycheck-haskell-stack-ghc-executable "@stack@/bin/stack")
 '(flycheck-c/c++-gcc-executable "@gcc@/bin/gcc")
 ;; '(flycheck-json-jsonlint-executable "@jsonlint@/bin/jsonlint")
 '(flycheck-javascript-eslint-executable "@eslint@/bin/eslint")
 '(flycheck-javascript-jshint-executable "@jshint@/bin/jshint")
 '(flycheck-go-build-executable "@go@/bin/go")
 '(flycheck-go-test-executable "@go@/bin/go")
 '(flycheck-lua-executable "@lua@/bin/luac")
 '(flycheck-xml-xmllint-executable "@libxml2@/bin/xmllint")
 '(flycheck-perl-perlcritic-executable "@perlcritic@/bin/perlcritic")
 '(flycheck-html-tidy-executable "@tidy@/bin/tidy")
 ;; TODO: add more flycheck executables
 '(fortune-dir "@fortune@/share/games/fortunes")
 '(fortune-file "@fortune@/share/games/fortunes/food")
 '(grep-program "@gnugrep@/bin/grep")
 '(ispell-program-name "@aspell@/bin/aspell")
 '(irony-cmake-executable "@cmake@/bin/cmake")
 '(jka-compr-info-compress-program "@ncompress@/bin/compress")
 '(jka-compr-info-uncompress-program "@ncompress@/bin/uncompress")
 '(irony-server-install-prefix "@irony@")
 '(jka-compr-dd-program "@coreutils@/bin/dd")
 '(jdee-server-dir "@jdeeserver@")
 '(magit-git-executable "@git@/bin/git")
 '(markdown-command "@markdown2@/bin/markdown2")
 '(manual-program "@man@/bin/man")
 '(man-awk-command "@gawk@/bin/awk")
 '(man-sed-command "@gnused@/bin/sed")
 '(man-untabify-command "@coreutils@/bin/pr")
 '(nethack-executable "@nethack@/bin/nethack")
 '(org-pandoc-command "@pandoc@/bin/pandoc")
 '(pandoc-binary "@pandoc@/bin/pandoc")
 '(remote-shell-program "@openssh@/bin/ssh")
 '(ripgrep-executable "@ripgrep@/bin/rg")
 '(rtags-path "@rtags@/bin")
 '(sql-ingres-program "@parallel@/bin/sql")
 '(sql-interbase-program "@unixODBC@/bin/isql")
 '(sql-mysql-program "@mariadb@/bin/mysql")
 '(sql-ms-program "@freetds@/bin/osql")
 '(sql-postgres-program "@freetds@/bin/osql")
 '(sql-sqlite-program "@sqliteInteractive@/bin/sqlite3")
 '(tramp-encoding-shell "@bash@/bin/sh")
 ;; '(tls-certtool-program "")
 ;; '(tramp-smb-program "")
 ;; '(tramp-smb-winexe "")
 ;; '(url-gateway-nslookup-program "")
 '(tex-shell "@bash@/bin/sh")
 '(xargs-program "@findutils@/bin/xargs")
 '(vc-git-program "@git@/bin/git")
 '(gnutls "@gnutls@/bin/gnutls-cli")
 '(pdf2dsc-command "@ghostscript@/bin/pdf2dsc")
 '(preview-gs-command "@texlive@/bin/rungs")
 '(TeX-command "@texlive@/bin/tex")
 '(LaTeX-command "@texlive@/bin/latex")
 '(luatex-command "@texlive@/bin/luatex")
 '(xetex-command "@texlive@/bin/xetex")
 '(xelatex-command "@texlive@/bin/xelatex")
 '(makeinfo-command "@texinfoInteractive@/bin/makeinfo")
 '(pdftex-command "@texlive@/bin/pdftex")
 '(context-command "@texlive@/bin/context")
 '(bibtex-command "@texlive@/bin/bibtex")
 '(dvipdfmx-command "@texlive@/bin/dvipdfmx")
 '(makeglossaries-command "@texlive@/bin/makeglossaries")
 '(makeindex-command "@texlive@/bin/makeindex")
 '(chktex-command "@texlive@/bin/chktex")
 '(lacheck-command "@texlive@/bin/lacheck")
 '(dvipdfmx-command "@texlive@/bin/dvipdfmx")
 '(dvips-command "@texlive@/bin/dvips")
 '(dvipng-command "@texlive@/bin/dvipng")
 '(ps2pdf-command "@ghostscript@/bin/ps2pdf")
 '(locate-executable "@findutils@/bin/locate")
 '(ag-executable "@ag@/bin/ag")
 '(intero-stack-executable "@intero@/bin/intero-nix-shim")
 '(notmuch-command "@notmuch@/bin/notmuch")
 )

(set-defaults
 '(imap-ssl-program `(,(concat gnutls " --tofu -p %p %s")))
 '(tls-program (concat gnutls " --tofu -p %p %h"))
 '(preview-pdf2dsc-command
   (concat pdf2dsc-command " %s.pdf %m/preview.dsc"))
 '(preview-dvips-command
   (concat dvips-command " -Pwww %d -o %m/preview.ps"))
 '(preview-fast-dvips-command
   (concat dvips-command " -Pwww %d -o %m/preview.ps"))
 '(preview-dvipng-command
   (concat dvipng-command
           " -picky -noghostscript %d -o \"%m/prev%%03d.png\""))
 '(TeX-engine-alist `((xetex "XeTeX" xetex-command xelatex-command
                             xetex-command)
                      (luatex "LuaTeX" luatex-command
                              ,(concat luatex-command " --jobname=%s")
                              luatex-command)))
 '(TeX-command-list
   `(("TeX"
      "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t"
      TeX-run-TeX nil
      (plain-tex-mode ams-tex-mode texinfo-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" ,(concat makeinfo-command " %(extraopts) %t")
      TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" ,(concat makeinfo-command " %(extraopts) --html %t")
      TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX"
      ,(concat pdftex-command " %(PDFout) %(extraopts) %`%S%(mode)%' %t")
      TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt"
      ,(concat context-command " --once --texutil %(extraopts) %(execopts)%t")
      TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" ,(concat context-command " %(extraopts) %(execopts)%t")
      TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" ,(concat bibtex-command " %s")
      TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t
      :help "View the printer queue" :visible TeX-queue-command)
     ("File" ,(concat dvips-command " %d -o %f ")
      TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" ,(concat dvips-command " %d -o %f ")
      TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" ,(concat dvipdfmx-command " %d")
      TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" ,(concat ps2pdf-command " %f")
      TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Glossaries" ,(concat makeglossaries-command " %s")
      TeX-run-command nil t :help "Run makeglossaries to create glossary file")
     ("Index" ,(concat makeindex-command " %s")
      TeX-run-index nil t :help "Run makeindex to create index file")
     ("upMendex" "upmendex %s"
      TeX-run-index t t :help "Run mendex to create index file")
     ("Xindy" "xindy %s"
      TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" ,(concat lacheck-command " %s") TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" ,(concat chktex-command " -v6 %s") TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")"
      TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean"
      TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t
      :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 ;; '(counsel-grep-base-command (concat grep-program " -nE '%s' %s"))
 '(counsel-grep-base-command
   (concat ripgrep-executable
           " -i -M 120 --no-heading --line-number --color never '%s' %s"))
 '(counsel-rg-base-command
   (concat ripgrep-executable " -i --no-heading --line-number %s ."))
 '(counsel-ag-base-command (concat ag-executable " --nocolor --nogroup %s"))
 '(org-preview-latex-process-alist
   `((dvipng :programs ("latex" "dvipng")
             :description "dvi > png"
             :message ""
             :image-input-type "dvi"
             :image-output-type "png"
             :image-size-adjust (1.0 . 1.0)
             :latex-compiler
             (,(concat LaTeX-command
                       " -interaction nonstopmode -output-directory %o %f"))
             :image-converter
             (,(concat dvipng-command
                       " -fg %F -bg %B -D %D -T tight -o %O %f")))))
 )

(eval-when-compile
  (require 'bind-key))

(bind-key "C-c C-u" 'rename-uniquely)
(bind-key "C-x ~" (lambda () (interactive) (find-file "~")))
(bind-key "C-x /" (lambda () (interactive) (find-file "/")))
(bind-key "C-c C-o" 'browse-url-at-point)
(bind-key "H-l" 'browse-url-at-point)
(bind-key "C-x 5 3" 'iconify-frame)
(bind-key "C-x 5 4" 'toggle-frame-fullscreen)
(bind-key "s-SPC" 'cycle-spacing)
(bind-key "C-c w w" 'whitespace-mode)

(bind-key "<C-return>" 'other-window)
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
(bind-key "C-c z" 'clean-buffer-list)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)

(bind-key "H-c" 'compile)
(bind-key "s-1" 'other-frame)
(bind-key "<s-return>" 'toggle-frame-fullscreen)

(bind-key "s-C-<left>" 'shrink-window-horizontally)
(bind-key "s-C-<right>" 'enlarge-window-horizontally)
(bind-key "s-C-<down>" 'shrink-window)
(bind-key "s-C-<up>" 'enlarge-window)

;; (autoload 'iso-transl-ctl-x-8-map
;;   "iso-transl" "Keymap for C-x 8 prefix." t 'keymap)
(require 'iso-transl)
(bind-key "' /" "′" iso-transl-ctl-x-8-map)
(bind-key "\" /" "″" iso-transl-ctl-x-8-map)
(bind-key "\" (" "“" iso-transl-ctl-x-8-map)
(bind-key "\" )" "”" iso-transl-ctl-x-8-map)
(bind-key "' (" "‘" iso-transl-ctl-x-8-map)
(bind-key "' )" "’" iso-transl-ctl-x-8-map)
(bind-key "4 < -" "←" iso-transl-ctl-x-8-map)
(bind-key "4 - >" "→" iso-transl-ctl-x-8-map)
(bind-key "4 b" "←" iso-transl-ctl-x-8-map)
(bind-key "4 f" "→" iso-transl-ctl-x-8-map)
(bind-key "4 p" "↑" iso-transl-ctl-x-8-map)
(bind-key "4 n" "↓" iso-transl-ctl-x-8-map)
(bind-key "<down>" "⇓" iso-transl-ctl-x-8-map)
(bind-key "<S-down>" "↓" iso-transl-ctl-x-8-map)
(bind-key "<left>" "⇐" iso-transl-ctl-x-8-map)
(bind-key "<S-left>" "←" iso-transl-ctl-x-8-map)
(bind-key "<right>" "⇒" iso-transl-ctl-x-8-map)
(bind-key "<S-right>" "→" iso-transl-ctl-x-8-map)
(bind-key "<up>" "⇑" iso-transl-ctl-x-8-map)
(bind-key "<S-up>" "↑" iso-transl-ctl-x-8-map)
(bind-key "," "…" iso-transl-ctl-x-8-map)

;; setup use-package and some extra
;; keywords for use-package-list.el
;; to work correctly
(eval-when-compile
  (defvar use-package-enable-imenu-support)
  (defvar use-package-expand-minimally)
  (defvar use-package-always-defer)

  (setq use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t)

  (autoload 'use-package-autoload-keymap "use-package")

  (require 'use-package)
  (require 'use-package-list))

;; some utils needed at init stage
;; should always appear before other use-package
(eval-when-compile
  (use-package add-hooks
    :commands (add-hooks add-hooks-pair))
  (use-package hook-helpers
    :commands (create-hook-helper
                define-hook-helper)
    :functions (make-hook-helper
                add-hook-helper
                hkhlp-normalize-hook-spec
                hkhlp-update-helper))
  )

(create-hook-helper save-on-unfocus ()
  :hooks (focus-out-hook)
  (save-some-buffers t))

(column-number-mode t)

(when (not (window-system))
  (xterm-mouse-mode +1))

;; Alphabetical listing of all packages

;; Run sort-package-declarations after adding a new package from this point
;; (make sure the provide line is still at the bottom though)

;; Each use-package call should be followed by a space to separate it
;; all comments must be within the sexp.

;; No packages on the top level should have the :demand keyword. Each package
;; should be setup as either commands, hooks, modes, or key bindings. Defer
;; timers are allowed but should be used sparingly. Currently, these packages
;; need defer timers:

;; - autorevert (1)
;; - company (2)
;; - delsel (2)
;; - dtrt-indent (3)
;; - flycheck (3)
;; - savehist (4)
;; - save-place (5)
;; - which-key (3)

(use-package ace-window
  :bind (("M-o" . other-window)
         ([remap next-multiframe-window] . ace-window)))

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
  :builtin)

(use-package apropospriate-theme
  :init
  (let ((filename (locate-library "apropospriate-theme")))
    (when filename
      (add-to-list 'custom-theme-load-path (file-name-directory filename)))))

(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package ansi-color
  :builtin
  :commands ansi-color-apply-on-region
  :init (create-hook-helper colorize-compilation-buffer ()
          :hooks (compilation-filter-hook)
          (let ((inhibit-read-only t))
            (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package anything
  :commands anything)

(use-package autorevert
  :builtin
  :demand
  :commands auto-revert-mode
  :init
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  :config
  (global-auto-revert-mode t))

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
  :init (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

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
    :init (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)))

(use-package coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)))

(use-package comint
  :builtin
  :bind
  (:map comint-mode-map
        ("C-r"       . comint-history-isearch-backward-regexp)
        ("s-k"       . comint-clear-buffer)
        ("M-TAB"     . comint-previous-matching-input-from-input)
        ("<M-S-tab>" . comint-next-matching-input-from-input))
  :commands (comint-next-prompt
             comint-write-input-ring
             comint-after-pmark-p
             comint-read-input-ring
             comint-send-input)
  :preface
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  :config
  (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  (create-hook-helper save-history ()
    :hooks (kill-emacs-hook)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (comint-write-input-ring)))))

(use-package company
  :demand
  :bind (:map company-active-map
              ;; ("TAB" . company-complete-common-or-cycle)
              ;; ("<tab>" . company-complete-common-or-cycle)
              ;; ("TAB" . company-select-next)
              ;; ("<tab>" . company-select-next)
              ("TAB" .
               company-select-next-if-tooltip-visible-or-complete-selection)
              ("<tab>" .
               company-select-next-if-tooltip-visible-or-complete-selection)
              ("S-TAB" . company-select-previous)
              ("<backtab>" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              )
  :commands (company-mode
             global-company-mode
             company-auto-begin
             company-complete-common-or-cycle)
  :config
  (setq company-backends
        '((company-css :with company-dabbrev)
          (company-nxml :with company-dabbrev)
          (company-elisp :with company-capf)
          (company-eshell-history :with company-capf company-files)
          (company-capf :with company-files company-keywords)
          (company-etags company-gtags company-clang company-cmake
                         :with company-dabbrev)
          (company-semantic :with company-dabbrev company-capf)
          (company-abbrev company-dabbrev company-keywords)
          ))
  (global-company-mode 1)
  (add-hook 'minibuffer-setup-hook 'company-mode)
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq-local company-frontends
                              '(company-preview-frontend))))
  (advice-add 'completion-at-point :override 'company-complete-common-or-cycle))

(use-package company-anaconda
  :commands company-anaconda
  :disabled
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-irony
  :disabled
  :after company
  :commands company-irony
  :config (add-to-list 'company-backends 'company-irony))

(use-package company-shell
  :disabled
  :after company
  :commands company-shell
  :config (add-to-list 'company-backends 'company-shell))

(use-package company-tern
  :disabled
  :after company
  :commands company-tern
  :config (add-to-list 'company-backends 'company-tern))

(use-package company-web
  :after company
  :disabled
  :commands (company-web-html company-web-slim company-web-jade)
  :config
  (add-to-list 'company-backends 'company-web-html)
  (add-to-list 'company-backends 'company-web-slim)
  (add-to-list 'company-backends 'company-web-jade))

(use-package compile
  :builtin
  :bind (("C-c C-c" . compile)
         ("M-O" . show-compilation)
         :map compilation-mode-map
         ("o" . compile-goto-error))
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

  :config
  (create-hook-helper compilation-ansi-color-process-output ()
    :hooks (compilation-filter-hook)
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker))))

(use-package copy-as-format
  :bind (("C-c w s" . copy-as-format-slack)
         ("C-c w g" . copy-as-format-github)))

(use-package counsel
  :commands (counsel-descbinds)
  :bind* (([remap execute-extended-command] . counsel-M-x)
          ([remap find-file] . counsel-find-file)
          ([remap describe-function] . counsel-describe-function)
          ([remap describe-variable] . counsel-describe-variable)
          ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
          ;; ([remap completion-at-point] . counsel-company)
          ([remap isearch-forward] . counsel-grep-or-swiper)
          ("<f1> l" . counsel-find-library)
          ("C-c j" . counsel-git-grep)
          ("C-c k" . counsel-ag)
          ("C-x l" . counsel-locate)
          ("C-M-i" . counsel-imenu)
          ("M-y" . counsel-yank-pop)
          ("C-c i 8" . counsel-unicode-char)
          ;; :map irony-mode-map
          ;; ([remap completion-at-point] . counsel-irony)
          ;; ([remap complete-symbol] . counsel-irony)
          )
  )

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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package dired
  :builtin
  :bind (("C-c J" . dired-double-jump)
         :map dired-mode-map
         ("C-c C-c" . compile)
         ("r" . browse-url-of-dired-file)
         ("e" . eshell)))

(use-package dired-collapse
  :disabled
  :after dired
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
  :commands (dired-omit-mode dired-hide-details-mode)
  :init
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  :bind (("s-\\" . dired-jump-other-window)
         :map dired-mode-map
         (")" . dired-omit-mode)))

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :demand
  :config (dtrt-indent-mode 1))

(use-package easy-kill
  :disabled
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package edebug
  :builtin)

(use-package eldoc
  :builtin
  :commands eldoc-mode
  :init
  (add-hooks '(((emacs-lisp-mode
                 eval-expression-minibuffer-setup
                 lisp-mode-interactive-mode
                 typescript-mode) . eldoc-mode))))

(use-package elec-pair ;; should disable in sp modes
  :builtin
  :commands electric-pair-mode
  :init
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (add-hook 'smartparens-mode-hook (lambda () (electric-pair-mode -1))))

(use-package electric ;; should disable in sp modes
  :builtin
  :commands (electric-quote-mode electric-indent-mode electric-layout-mode)
  :init
  (add-hook 'prog-mode-hook 'electric-quote-mode)
  (add-hook 'prog-mode-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook 'electric-layout-mode))

(use-package elf-mode
  :commands elf-mode
  ;; TODO: use :magic
  :init (add-to-list 'magic-mode-alist (cons "ELF" 'elf-mode)))

(use-package elfeed
  :commands elfeed)

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode))

(use-package em-dired
  :commands (em-dired-mode em-dired-new)
  :init
  (add-hook 'eshell-mode-hook 'em-dired-mode)
  (advice-add 'eshell :before 'em-dired-new)
  ;; in local dir
  :builtin)

(use-package emacs-lisp-mode
  :builtin
  :interpreter (("emacs" . emacs-lisp-mode)))

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
  :commands (eshell eshell-command eshell-bol)
  :preface
  (defun eshell-eol ()
    "Goes to the end of line."
    (interactive)
    (end-of-line))
  :init
  (defvar eshell-rebind-keys-alist)
  (add-to-list 'eshell-rebind-keys-alist '([(control 101)] . eshell-eol))
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
          eshell-xtra)))

(use-package ess-site
  :name "ess"
  :commands R)

(use-package esup
  :commands esup)

(use-package etags
  :builtin
  :commands (tags-completion-table))

(use-package executable
  :builtin
  :commands executable-make-buffer-file-executable-if-script-p
  :init
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package firestarter
  :bind ("C-c m s" . firestarter-mode))

(use-package flycheck
  :demand
  :commands global-flycheck-mode
  :config (global-flycheck-mode))

(use-package flycheck-irony
  :commands flycheck-irony-setup
  :init (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package flyspell
  :builtin
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package focus
  :bind ("C-c m f" . focus-mode))

(use-package ggtags
  :builtin)

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
  :commands (goto-address-prog-mode goto-address-mode)
  :init
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

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
         ;; ("<f10>" . gud-next)
         ;; ("<f11>" . gud-step)
         ("S-<f11>" . gud-finish)))

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :commands haskell-indentation-moe
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  :config
  (require 'haskell-doc))

(use-package hideshow
  :disabled
  :builtin
  :commands hs-minor-mode
  :init (add-hooks '(((c-mode-common
                       lisp-mode
                       emacs-lisp-mode
                       java-mode) . hs-minor-mode))))

(use-package hideshowvis
  :disabled
  :commands (hideshowvis-minor-mode hideshowvis-symbols)
  :init (add-hook 'prog-mode-hook 'hideshowvis-minor-mode))

(use-package hippie-exp
  :builtin
  :bind* (("M-/". hippie-expand)))

(use-package hl-todo
  ;; TODO: add font-lock highlighting for @nethack@ substitutions
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

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
      ("q" nil "quit"))))

(use-package ibuffer
  :builtin
  :bind ([remap switch-to-buffer] . ibuffer))

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
         :map isearch-mode-map ("C-;" . iedit-mode-toggle-on-function)))

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
  :disabled
  :builtin
  :bind ("C-h C-i" . info-lookup-symbol)
  )

(use-package intero
  :commands intero-mode
  :preface
  (defun intero-mode-unless-global-project ()
    "Run intero-mode iff we're in a project with a stack.yaml"
    (unless (string-match-p
             (regexp-quote ".stack/global-project")
             (shell-command-to-string
              "stack path --project-root --verbosity silent"))
      (intero-mode)))
  :init
  ;; (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook 'intero-mode-unless-global-project)
  )

(use-package irony
  :commands irony-mode
  :init (add-hooks '(((c++-mode c-mode objc-mode) . irony-mode))))

(use-package irony-eldoc
  :commands irony-eldoc
  :init (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package ivy
  :diminish ivy-mode
  :bind (("<f6>" . ivy-resume)
         ([remap list-buffers] . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ;; ("C-j" . ivy-call)
         ("<escape>" . abort-recursive-edit))
  :commands ivy-mode
  :init
  (defvar projectile-completion-system)
  (defvar magit-completing-read-function)
  (defvar dumb-jump-selector)
  (defvar rtags-display-result-backend)
  (defvar projector-completion-system)
  (setq projectile-completion-system 'ivy
        magit-completing-read-function 'ivy-completing-read
        dumb-jump-selector 'ivy
        rtags-display-result-backend 'ivy
        projector-completion-system 'ivy)
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
  (jka-compr-update))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  ;; :interpreter (("node" . js2-mode""))
  :commands js2-imenu-extras-mode
  :init
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package js3-mode
  :commands js3-mode)

(use-package json-mode
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . json-mode))
  :config
  (make-local-variable 'js-indent-level))

(use-package keyfreq
  :disabled
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package kill-or-bury-alive
  :bind (([remap kill-buffer] . kill-or-bury-alive)))

(use-package llvm-mode
  :mode "\\.ll\\'")

(use-package lsp-mode
  :disabled
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
    :demand))

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
  (add-to-list 'magic-mode-alist '("\xCF\xFA\xED\xFE" . macho-mode)))

(use-package magit
  :preface
  (defun magit-dired-other-window ()
    (interactive)
    (dired-other-window (magit-toplevel)))

  (defun magit-remote-github (username &optional args)
    (interactive (list (magit-read-string-ns "User name")
                       (magit-remote-arguments)))
    (let* ((url (magit-get "remote.origin.url"))
           (match (string-match "^https?://github\.com/[^/]*/\\(.*\\)" url)))
      (unless match
        (error "Not a github remote"))
      (let ((repo (match-string 1 url)))
        (apply 'magit-remote-add username (format "https://github.com/%s/%s"
                                                  username repo) args))))

  :commands (magit-clone
             magit-toplevel
             magit-read-string-ns
             magit-remote-arguments
             magit-get
             magit-remote-add
             magit-define-popup-action)

  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup)
         :map magit-mode-map
         ("C-o" . magit-dired-other-window))
  :init
  (defvar magit-last-seen-setup-instructions "1.4.0")
  :config
  (create-hook-helper magit-github-hook ()
    :hooks (magit-mode-hook)
    (magit-define-popup-action 'magit-remote-popup
      ?g "Add remote from github user name" #'magit-remote-github)))

(use-package magithub
  :disabled
  :commands magithub-feature-autoinject
  :init (add-hook 'magit-mode-hook 'magithub-feature-autoinject))

(use-package make-it-so
  :disabled
  :commands mis-mode
  :init (add-hook 'dired-mode-hook 'mis-mode)
  (bind-keys :map dired-mode-map
             :prefix ","
             :prefix-map dired-make-it-so-map
             :prefix-docstring "Make it so map."
             ("," . make-it-so)
             ("f" . mis-finalize)
             ("a" . mis-abort)
             ("r" . mis-replace))
  (use-package make-mode
    :bind (:map makefile-mode-map ("<f5>" . mis-save-and-compile))))

(use-package makefile-mode
  :builtin
  :init
  (add-hook 'makefile-mode-hook 'indent-tabs-mode))

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :config
  (bind-key "'" "’" markdown-mode-map
            (not (or (markdown-code-at-point-p)
                     (memq 'markdown-pre-face
                           (face-at-point nil 'mult))))))

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

(use-package multi-term
  :bind (("C-. t" . multi-term-next)
         ("C-. T" . multi-term)))

(use-package multiple-cursors
  :bind
  (("<C-S-down>" . mc/mark-next-like-this) ;; broken by macOS shortcut
   ("<C-S-up>" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("M-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-!"         . mc/mark-next-symbol-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package newcomment
  :builtin
  :bind ("s-/" . comment-or-uncomment-region))

(use-package nix-buffer
  :commands nix-buffer)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-fontify
  :builtin
  :commands nix-fontify-mode)

(use-package notmuch
  :commands notmuch)

(use-package nroff-mode
  :builtin
  :commands nroff-mode)

(use-package nxml-mode
  :builtin
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode))

(use-package org
  :builtin
  ;; :mode "\\.\\(org\\)\\'"
  :commands org-capture
  :bind* (("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link)
          ("C-c b" . org-iswitchb))
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        'pcomplete-completions-at-point nil t)))
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
  (use-package ox-mediawiki
    :demand)
  (use-package ox-pandoc
    :demand)
  (use-package ox-reveal
    :demand)
  (use-package ox-ref
    :disabled
    :demand)
  (use-package ox-beamer
    :builtin
    :demand))

(use-package org-bullets
  :disabled
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package pabbrev
  :commands pabbrev-mode)

(use-package page-break-lines
  :commands page-break-lines-mode
  :init (add-hooks '(((doc-mode
                       help-mode
                       emacs-lisp-mode) . page-break-lines-mode))))

(use-package pandoc-mode
  :commands (pandoc-mode pandoc-load-default-settings)
  :init
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;; (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package php-mode
  :mode "\\.php\\'")

(use-package pp
  :builtin
  :commands pp-eval-last-sexp
  :bind (([remap eval-expression] . pp-eval-expression))
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
  ;; (global-prettify-symbols-mode)
  (create-hook-helper prettify-symbols-prog ()
    ""
    :hooks (prog-mode-hook)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-lisp ()
    ""
    :hooks (lisp-mode-hook)
    (push '("/=" . ?≠) prettify-symbols-alist)
    (push '("sqrt" . ?√) prettify-symbols-alist)
    (push '("not" . ?¬) prettify-symbols-alist)
    (push '("and" . ?∧) prettify-symbols-alist)
    (push '("or" . ?∨) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-c ()
    ""
    :hooks (c-mode-hook)
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("!=" . ?≠) prettify-symbols-alist)
    (push '("&&" . ?∧) prettify-symbols-alist)
    (push '("||" . ?∨) prettify-symbols-alist)
    (push '(">>" . ?») prettify-symbols-alist)
    (push '("<<" . ?«) prettify-symbols-alist))
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
    (push '("->" . ?→) prettify-symbols-alist))
  (create-hook-helper prettify-symbols-js ()
    ""
    :hooks (js2-mode-hook js-mode-hook)
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("=>" . ?⇒) prettify-symbols-alist)))

(use-package projectile
  ;; :bind ("s-f" . hydra-projectile/body)
  :bind-keymap* (("C-c p" . projectile-command-map)
                 ("s-p" . projectile-command-map))
  :commands (projectile-mode)
  :defer 1
  :config
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-compilation-cmd 'safe-local-variable
       (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command))
                                   compilation-read-command))))

  (projectile-mode)

  (use-package easymenu
    :builtin
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

(use-package readline-complete
  :disabled
  :after company
  :config
  (add-to-list 'company-backends 'company-readline)
  (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1))))

(use-package realgud
  :commands (realgud:jdb))

(use-package repl-toggle
  :disabled
  :config
  (repl-toggle-mode)
  (setq rtog/mode-repl-alist
        '((emacs-lisp-mode . ielm)
          (ruby-mode . inf-ruby)
          (js2-mode . nodejs-repl)
          (rjsx-mode . nodejs-repl))))

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
  :demand
  :commands savehist-mode
  :config (savehist-mode 1))

(use-package saveplace
  :builtin
  :disabled
  :commands save-place-mode
  :demand
  :config (save-place-mode t))

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package server
  :disabled
  :builtin
  :demand
  :commands server-start
  :config
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'server-switch-hook 'raise-frame))

(use-package shrink-whitespace
  :bind ("H-SPC" . shrink-whitespace))

(use-package sh-script
  :builtin
  :preface
  (defun shell-command-at-point ()
    (interactive)
    (let ((start-point (save-excursion
                         (beginning-of-line)
                         (point))))
      (shell-command (buffer-substring start-point (point)))))
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :bind (:map sh-mode-map
              ("C-x C-e" . shell-command-at-point)))

(use-package shell
  :builtin
  :commands (shell shell-mode)
  :bind ("C-c C-s" . shell)
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'dirtrack-mode)
  (create-hook-helper use-histfile ()
    :hooks (shell-mode-hook)
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package simple
  :builtin
  :bind
  (("C-`" . list-processes)
   :map minibuffer-local-map
   ("<escape>"  . abort-recursive-edit)
   ("M-TAB"     . previous-complete-history-element)
   ("<M-S-tab>" . next-complete-history-element))
  :commands visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode))

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
  (add-hook 'text-mode-hook 'smart-hungry-delete-default-text-mode-hook))

(use-package smart-shift
  :bind (("C-c <left>" . smart-shift-left)
         ("C-c <right>" . smart-shift-right)
         ("C-c <up>" . smart-shift-up)
         ("C-c <down>" . smart-shift-down)))

(use-package smartparens
  :commands (smartparens-mode
             show-smartparens-mode
             smartparens-strict-mode
             sp-local-tag
             sp-local-pair)
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
              ("C-}" . sp-forward-barf-sexp)
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
              :map smartparens-strict-mode-map
              ([remap c-electric-backspace] . sp-backward-delete-char)
              :map emacs-lisp-mode-map
              (";" . sp-comment))
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

  (sp-with-modes 'org-mode
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
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap)))

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
  :commands subword-mode
  :init (add-hook 'java-mode-hook 'subword-mode))

(use-package sudo-edit
  :bind (("C-c C-r" . sudo-edit)))

(use-package swiper
  ;; :bind (([remap isearch-forward] . swiper)
  ;;        ([remap isearch-backward] . swiper))
  )

(use-package term
  :builtin
  :commands (term-mode term-char-mode term-set-escape-char)
  :init
  (add-hook 'term-mode-hook (lambda ()
                              (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
                              (setq-local transient-mark-mode nil)
                              (auto-fill-mode -1)))
  :preface
  (defun my-term ()
    (interactive)
    (set-buffer (make-term "my-term" "zsh"))
    (term-mode)
    ;; (term-line-mode)
    (term-char-mode)
    (term-set-escape-char ?\C-x)
    (switch-to-buffer "*my-term*"))
  :bind ("C-c t" . my-term))

(use-package tern
  :commands tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode))

(use-package tex-site
  :name "auctex"
  :commands (TeX-latex-mode
             TeX-mode
             tex-mode
             LaTeX-mode
             latex-mode)
  :mode ("\\.tex\\'" . TeX-latex-mode))

(use-package texinfo
  :mode ("\\.texi\\'" . texinfo-mode))

(use-package text-mode
  :builtin
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill))

(use-package tide
  :commands (tide-setup tide-hl-identifier-mode)
  :init
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package tramp
  :builtin
  :commands (tramp-tramp-file-p
             tramp-file-name-user
             tramp-file-name-real-host
             tramp-dissect-file-name))

(use-package transpose-frame
  :bind ("H-t" . transpose-frame))

(use-package try
  :commands try)

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package undo-tree
  :disabled
  :config (global-undo-tree-mode 1)
  :bind (("C-c u" . undo-tree-visualize)
         ("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo))
  :diminish undo-tree-mode)

(use-package vkill
  :bind ("C-x L" . vkill))

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)))

(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :demand
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
  :bind (("<s-down>" . windmove-down)
         ("<s-up>" . windmove-up)
         ;; ("<s-left>" . windmove-left)
         ;; ("<s-right>" . windmove-right)
         ))

(use-package view
  :builtin
  :bind (:map view-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("j" . next-line)
              ("k" . previous-line)
              ("l" . forward-char)
              ("f" . forward-char)
              ("b" . backward-char)))

(use-package vmd-mode
  :bind (:map markdown-mode-map ("C-x p" . vmd-mode)))

(use-package xterm-color
  :commands xterm-color-filter
  :init
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package yasnippet
  :disabled ;; haven’t found yasnippet very useful
  :commands yas-minor-mode
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config (yas-reload-all))

(use-package ycmd
  :disabled
  :commands global-ycmd-mode
  :init
  (add-hook 'after-init-hook #'global-ycmd-mode)
  :config
  (add-hook 'ycmd-file-parse-result-hook 'flycheck-ycmd--cache-parse-results)
  (use-package ycmd-eldoc
    :builtin
    :demand
    :config
    (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))
  (use-package flycheck-ycmd
    :builtin
    :demand
    :config
    (flycheck-ycmd-setup))
  (use-package company-ycmd
    :builtin
    :demand
    :config
    (company-ycmd-setup))
  )

(use-package mediawiki)

(use-package counsel-projectile
  :disabled
  :commands counsel-projectile-on
  :init (add-hook 'projectile-mode-hook 'counsel-projectile-on))

(use-package company-statistics
  :commands company-statistics-mode
  :init (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-auctex
  :disabled
  :commands (company-auctex-labels
             company-auctex-bibs
             company-auctex-macros
             company-auctex-symbols
             company-auctex-environments)
  :after company
  :config
  (add-to-list 'company-backends 'company-auctex-labels)
  (add-to-list 'company-backends 'company-auctex-bibs)
  (add-to-list 'company-backends
               '(company-auctex-macros
                 company-auctex-symbols
                 company-auctex-environments)))

(use-package company-jedi
  :disabled
  :after company
  :commands company-statistics-mode
  :init (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package checkbox
  :disabled
  :bind (("C-c C-t" . checkbox-toggle)))

(use-package yafolding
  :commands yafolding-mode
  :init (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package nethack
  :commands nethack
  :builtin)

(use-package cider)

(use-package xah-math-input
  :commands xah-math-input-mode)

(use-package ffap
  :builtin
  ;; TODO: handle line numbers like filename:line:col
  )

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

(use-package pabbrev
  :disabled
  :commands pabbrev-mode
  :init (add-hook 'prog-mode-hook 'pabbrev-mode))

(use-package company-eshell-history
  :builtin
  :commands company-eshell-history
  )

(use-package auto-compile
  :disabled
  :demand t
  :init
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest t)
  (setq auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package tooltip
  :builtin
  :demand
  :config
  (tooltip-mode -1))

(use-package ws-butler
  :disabled
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package help
  :builtin
  :bind (:map help-map
              ("C-v" . find-variable)
              ("C-k" . find-function-on-key)
              ("C-f" . find-function)
              ("C-l" . find-library)
              :map help-mode-map
              ("g" . revert-buffer-no-confirm))
  :preface
  (defun revert-buffer-no-confirm (&optional ignore-auto)
    "Revert current buffer without asking."
    (interactive (list (not current-prefix-arg)))
    (revert-buffer ignore-auto t nil)))

(use-package dired-imenu
  :after dired)

(provide 'default)
;;; default.el ends here
