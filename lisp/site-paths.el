;;; site-paths.el --- Site-specific paths

;;; Commentary:

;;; Code:

(require 'set-defaults)
(require 'subr-x)


;; IMPURE: reference to nix-profile!
(defvar output-directory (expand-file-name ".nix-profile" (getenv "HOME")))

(setq
 ;; ideally this would just use $out
 ;; but we need emacs to be built first
 ;; maybe in the future
 exec-path (append `(,(expand-file-name "bin" output-directory)

                     ;; handle /usr/bin/open and other system-specific stuff
                     "/usr/sbin" "/usr/bin"
                     "/sbin" "/bin"
                     )
                   exec-path)
 )

(defvar man-path `("/usr/share/man"
                   "/usr/local/share/man"
                   ,(expand-file-name "share/man" output-directory)))

(defcustom cacert-file "/etc/ssl/certs/ca-bundle.crt"
  "Path for SSL certificates.")

(set-envs
 `("NIX_SSL_CERT_FILE" ,cacert-file)
 `("NIX_PATH" ,(concat
                "nixpkgs=/nix/var/nix/profiles/per-user/"
                (getenv "USER")
                "/channels/nixpkgs"))
 `("PATH" ,(string-join exec-path ":"))
 `("MANPATH" ,(string-join man-path ":"))
 )

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
 '(cacert-file "@cacert@/etc/ssl/certs/ca-bundle.crt")
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
 '(Info-directory-list `(,(expand-file-name "share/info" output-directory)))
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
 )

(provide 'site-paths)
;;; site-paths.el ends here
