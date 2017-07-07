{
  packageOverrides = pkgs: with pkgs; rec {
    nix = nixStable;

    rEnv = pkgs.rWrapper.override {
      packages = with pkgs.rPackages; [
        # devtools
        # ggplot2
        # yaml
        # optparse
        RCurl
      ];
    };

    customEmacsPackages = emacsPackagesNg.overrideScope (super: self: {
      emacs = emacs;
    });

    myAspell = runCommand "aspell" { buildInputs = [ makeWrapper ]; } ''
      mkdir -p $out/bin
      makeWrapper ${aspell}/bin/aspell $out/bin/aspell \
        --add-flags "--conf=${writeText "aspell.conf" ''
dict-dir ${aspellDicts.en}/lib/aspell
        ''}"
    '';

    jdee-server = stdenv.mkDerivation {
      src = fetchFromGitHub {
        owner = "jdee-emacs";
        repo = "jdee-server";
        rev = "446b262bdacb88e68f81af601a2ee0adfea41e24";
        sha256 = "0qakixx9cvd7m1dsilmwq99gk3g10mfxvf19pqkq24vvixavd8w5";
      };
      name = "jdee-server";
      buildInputs = [ maven ];
      buildPhase = "mvn -DskipTests=true -Dmaven.repo.local=$(pwd) assembly:assembly";
      installPhase = "mkdir $out; cp target/jdee-bundle-*.jar $out";
    };

    myEmacs = customEmacsPackages.emacsWithPackages (epkgs:
      let pkgs = ([
        nix
        ghc
        rtags
        # mu
        notmuch
      ] ++ (with emacsPackages; [
        proofgeneral
        ess
      ]) ++ (with epkgs.elpaPackages; [
        ace-window
        aggressive-indent
        all
        auctex
        avy
        bug-hunter
        chess
        coffee-mode
        company
        csv-mode
        diff-hl
        dired-du
        docbook
        electric-spacing
        ggtags
        gnorb
        gnugo
        hydra
        hook-helpers
        ivy
        js2-mode
        json-mode
        minimap
        muse
        multishell
        org
        other-frame-window
        python
        rainbow-mode
        realgud
        sed-mode
        svg
        undo-tree
        vlf
        w3
        windresize
      ]) ++ (with epkgs.melpaStablePackages; [
        ag
        bind-key
        buffer-move
        bury-successful-compilation
        counsel
        company-irony
        diminish
        dumb-jump
        elpy
        esup
        expand-region
        flycheck
        flycheck-irony
        gist
        go-mode
        haml-mode
        haskell-mode
        iedit
        imenu-anywhere
        imenu-list
        intero
        irony
        indium
        less-css-mode
        lua-mode
        magit
        markdown-mode
        mmm-mode
        multiple-cursors
        mwim
        neotree
        org-bullets
        page-break-lines
        php-mode
        projectile
        projectile
        rainbow-delimiters
        restart-emacs
        rust-mode
        sass-mode
        scss-mode
        shut-up
        smart-tabs-mode
        smartparens
        swiper
        tern
        toc-org
        use-package
        web-mode
        which-key
        whitespace-cleanup-mode
        wrap-region
        xterm-color
        yaml-mode
        helm
        rg
        go-eldoc
        indium
        kill-or-bury-alive
        elpy
        counsel-dash
        mediawiki
        move-text
        counsel-dash
        crux
        keyfreq
        multi-term
      ]) ++ (with epkgs.melpaPackages; [
        apropospriate-theme
        browse-at-remote
        c-eldoc
        css-eldoc
        dtrt-indent
        esh-help
        eshell-prompt-extras
        jdee
        sudo-edit
        smart-hungry-delete
        try
      ])); in pkgs ++ [(runCommand "default.el" {
        inherit rtags ripgrep ag emacs ant nethack fortune gnutls;
        gpg = gnupg1compat;
        jdeeserver = jdee-server;
        aspell = myAspell;
      } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${./default.el} $out/share/emacs/site-lisp/default.el
          substituteAllInPlace $out/share/emacs/site-lisp/default.el

          # loadPaths=""
          # for f in ${toString pkgs}; do
          #   loadPaths="$loadPaths -L $f/share/emacs/site-lisp/elpa/* -L $f/share/emacs/site-lisp"
          # done
          # $emacs/bin/emacs --batch $loadPaths -f batch-byte-compile "$out/share/emacs/site-lisp/default.el"
        '')]
      );

    bauer = buildEnv {
      buildInputs = [ makeWrapper ];
      postBuild = ''
        if [ -w $out/share/info ]; then
           shopt -s nullglob
           for i in $out/share/info/*.info $out/share/info/*.info.gz; do # */
             ${texinfoInteractive}/bin/install-info $i $out/share/info/dir
           done
        fi

        mkdir -p $out/etc

        cp ${./gitconfig} $out/etc/gitconfig
        substituteInPlace $out/etc/gitconfig \
          --replace @gitignore@ ${./gitignore} \
          --replace @gnupg@ ${gnupg1compat}/bin/gpg \
          --replace @out@ $out

        cp ${./bashrc.sh} $out/etc/bashrc
        substituteInPlace $out/etc/bashrc \
          --replace @fortune@ ${fortune} \
          --replace @out@ $out

        cp ${./zshrc.sh} $out/etc/.zshrc
        substituteInPlace $out/etc/.zshrc \
          --replace @zsh-autosuggestions@ ${zsh-autosuggestions} \
          --replace @fortune@ ${fortune} \
          --replace @out@ $out
        cp $out/etc/.zshrc $out/etc/zshrc

        cp ${./etc-profile.sh} $out/etc/profile
        substituteInPlace $out/etc/profile \
          --replace @out@ $out

        wrapProgram $out/bin/bash \
          --add-flags "--rcfile $out/etc/bashrc"

        wrapProgram $out/bin/zsh \
          --set ZDOTDIR $out/etc
      '';
      meta.priority = 10;
      pathsToLink = [
        "/bin"
        "/etc/profile.d"
        "/etc/bash_completion.d"
        "/Applications"
        "/share/doc"
        "/share/man"
        "/share/info"
        "/share/zsh"
        "/share/bash-completion"
      ];
      extraOutputsToInstall = [ "man" "info" "doc" "devdoc" "devman" ];
      name = "bauer";
      paths = [
        bash-completion
        zsh-completions
        myAspell
        myEmacs
        gcc
        gawk
        bashInteractive
        bc
        bzip2
        cabal-install
        cabal2nix
        cargo
        checkbashisms
        cmake
        coreutils
        diffutils
        editorconfig-core-c
        emscripten
        ffmpeg
        findutils
        ripgrep
        ag
        ghc
        git
        gitAndTools.hub
        go2nix
        gnugrep
        gnumake
        # offlineimap
        gnuplot
        gnused
        gnupg1compat
        gnutar
        gnutls
        go
        gzip
        jdk
        jq
        haskellPackages.intero
        lua
        less
        man
        nano
        nasm
        nox
        nix
        nix-prefetch-scripts
        # nix-index
        nix-repl
        nix-zsh-completions
        ninja
	rtags
        nmap
        nodePackages.tern
        nodejs
        openssh
        openssl
        pandoc
        patch
        pypi2nix
        python
        perl
        php
        pwgen
        rsync
        ruby
        rustc
        screen
        stack
        time
        tree
        unzip
        vim
        w3m
        wget
        v8
        xz
        zip
        zsh
        fortune
        rEnv
        isync
        # mu
        notmuch
        (runCommand "my-profile" { buildInputs = [makeWrapper]; } ''
          mkdir -p $out/etc/profile.d
          cp ${./profile.sh} $out/etc/profile.d/my-profile.sh
          substituteInPlace $out/etc/profile.d/my-profile.sh \
            --replace @emacs@ ${myEmacs} \
            --replace @fortune@ ${fortune} \
            --replace @cacert@ ${cacert}
        '')
      ];
    };

    userPackages = bauer;
  };
}
