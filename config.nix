{
  packageOverrides = pkgs: with pkgs; rec {
    customEmacsPackages = emacsPackagesNg.overrideScope (super: self: {
      emacs = emacsMacport;
    });
    myEmacs = customEmacsPackages.emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
      nixStable
      ghc

      ace-jump-mode
      ace-window
      ag
      aggressive-indent
      apropospriate-theme
      avy
      buffer-move
      coffee-mode
      company
      company-flx
      counsel
      counsel-projectile
      crontab-mode
      diffview
      dumb-jump
      esh-help
      eshell-fringe-status
      eshell-prompt-extras
      expand-region
      esup
      flycheck
      gist
      gitattributes-mode
      gitconfig-mode
      github-clone
      gitignore-mode
      go-eldoc
      go-mode
      golden-ratio
      haml-mode
      haskell-mode
      iedit
      imenu-anywhere
      imenu-list
      indium
      intero
      ivy
      flx
      less-css-mode
      lua-mode
      magit
      magit-gh-pulls
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
      # rtags
      rust-mode
      sass-mode
      scss-mode
      smart-tabs-mode
      smartparens
      swiper
      tern
      tiny
      toc-org
      transpose-frame
      use-package
      web-mode
      which-key
      whitespace-cleanup-mode
      wrap-region
      xterm-color
      (runCommand "default.el" {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp ${./default.el} $out/share/emacs/site-lisp/default.el
      '')
    ]));
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
        # fasd
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
        # mu
        mutt
        mplayer
        mariadb
        moreutils
        # notmuch
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
        # rtags
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
