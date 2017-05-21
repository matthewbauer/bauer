{
  packageOverrides = pkgs: with pkgs; rec {
    customEmacsPackages = emacsPackagesNg.overrideScope (super: self: {
      emacs = emacsMacport;
    });
    myEmacs = customEmacsPackages.emacsWithPackages (epkgs:
      [
        (runCommand "default.el" {} ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${./default.el} $out/share/emacs/site-lisp/default.el
        '')
        nixStable
        ghc
        # rtags
        # mu
        # notmuch
      ]
      ++ (with epkgs; [
        avy
        company
        counsel
        flx
        flycheck
        go-mode
        haml-mode
        haskell-mode
        ivy
        projectile
        swiper
        use-package
      ])
      ++ (with epkgs.melpaStablePackages; [
        ace-jump-mode
        ace-window
        ag
        aggressive-indent
        buffer-move
        coffee-mode
        diffview
        dumb-jump
        expand-region
        esup
        gist
        gitattributes-mode
        gitconfig-mode
        github-clone
        gitignore-mode
        go-eldoc
        golden-ratio
        iedit
        imenu-anywhere
        imenu-list
        indium
        intero
        less-css-mode
        lua-mode
        markdown-mode
        multi-line
        multiple-cursors
        mwim
        neotree
        org-bullets
        page-break-lines
        php-mode
        projectile
        rainbow-delimiters
        restart-emacs
        rust-mode
        sass-mode
        scss-mode
        smart-tabs-mode
        smartparens
        swiper
        tern
        toc-org
        web-mode
        which-key
        whitespace-cleanup-mode
        wrap-region
        xterm-color
      ])
      ++ (with epkgs.melpaPackages; [
        apropospriate-theme
        company-flx
        counsel-projectile
        crontab-mode
        esh-help
        eshell-fringe-status
        eshell-prompt-extras
        magit
        magit-gh-pulls
        tiny
        transpose-frame
      ])
    );
    userPackages = buildEnv {
      postBuild =
        ''
          if [ -x $out/bin/install-info -a -w $out/share/info ]; then
            shopt -s nullglob
            for i in $out/share/info/*.info $out/share/info/*.info.gz; do # */
                $out/bin/install-info $i $out/share/info/dir
            done
          fi
        '';
      pathsToLink = [
        "/bin"
        "/etc/profile.d"
        "/Applications"
        "/share/doc"
        "/share/man"
        "/share/info"
        "/share/zsh"
        "/share/bash-completion"
        "/share/hunspell"
      ];
      extraOutputsToInstall = [ "man" "info" "doc" "devdoc" ];
      name = "user-packages";
      paths = [
        myEmacs
        aspell
        anki
        bashInteractive
        bc
        bison
        bzip2
        cabal-install
        cabal2nix
        cacert
        cargo
        checkbashisms
        cmake
        coreutils
        curl
        clang
        ctags
        # darwin.DarwinTools
        # darwin.basic_cmds
        # darwin.developer_cmds
        diffutils
        editorconfig-core-c
        emscripten
        ffmpeg
        fetchmail
        findutils
        gcc
        geany
        gdb
        ghc
        git
        gitAndTools.hub
        gimp
        groff
        go2nix
        gnugrep
        gnumake
        gnuplot
        gnused
        gnupg1compat
        gnutar
        gnutls
        go
        gzip
        imagemagick
        jq
        haskellPackages.intero
        haskellPackages.ghc-mod
        hunspell
        hunspellDicts.en-us
        html-tidy
        lua
        less
        man
        myEmacs
        mutt
        mplayer
        mariadb
        moreutils
        nano
        nasm
        nox
        # netcat
        # nixUnstable
        nixStable
        nix-prefetch-scripts
        # nix-index
        nix-repl
        nix-zsh-completions
        ninja
        nmap
        nodePackages.tern
        nodejs
        # node2nix
        oh-my-zsh
        openssh
        openssl
        pandoc
        patch
        pypi2nix
        python
        perl
        php
        pwgen
        qemu
        ripgrep
        rsync
        ruby
        rustc
        retroarch
        screen
        silver-searcher
        stack
        subversion
        time
        texinfoInteractive
        tree
        transmission
        # unrar
        unzip
        vim
        w3m
        wget
        weechat
        # v8
        xz
        ycmd
        zip
        zsh
        jdk
        (runCommand "profile" {} ''
mkdir -p $out/etc/profile.d
cp ${./my-profile.sh} $out/etc/profile.d/my-profile.sh
        '')
      ];
    };
  };
}
