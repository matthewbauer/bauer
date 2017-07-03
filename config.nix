{
  packageOverrides = pkgs: with pkgs; rec {
    nix = nixStable;

    myConfig = {
      gitconfig = ./gitconfig;
      gitignore = ./gitignore;
      zshrc = ./zshrc.sh;
      bashrc = ./bashrc.sh;
      profile = ./profile.sh;
      etc-profile = ./etc-profile.sh;
      emacs = ./default.el;
    };

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
        mu
      ]
      ++ (with emacsPackages; [
        proofgeneral
        ess
      ])
      ++ (with epkgs.elpaPackages; [
        ace-window
        aggressive-indent
        all
        auctex
        avy
        bug-hunter
        chess
        coffee-mode
        company
        dired-du
        docbook
        easy-kill
        electric-spacing
        excorporate
        ggtags
        gnorb
        gnugo
        ivy
        js2-mode
        json-mode
        minimap
        muse
        org
        other-frame-window
        python
        rainbow-mode
        realgud
        tiny
        undo-tree
        vlf
        w3
        windresize
      ])
      ++ (with epkgs.melpaStablePackages; [
        ace-jump-mode
        ag
        bind-key
        buffer-move
        counsel
        diffview
        diminish
        dumb-jump
        esup
        expand-region
        flx
        flycheck
        gist
        go-mode
        haml-mode
        haskell-mode
        iedit
        imenu-anywhere
        imenu-list
        intero
        irony
        less-css-mode
        lua-mode
        magit
        markdown-mode
        mmm-mode
        multi-line
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
        bm
        diff-hl
        hydra
        counsel-dash
      ])
      ++ (with epkgs.melpaPackages; [
        apropospriate-theme
        autodisass-java-bytecode
        bury-successful-compilation
        c-eldoc
        css-eldoc
        diff-hl
        dtrt-indent
        elpy
        esh-help
        eshell-prompt-extras
        flycheck-irony
        go-eldoc
        hideshowvis
        indium
        irony
        jdee
        kill-or-bury-alive
        mediawiki
        noflet
        smart-hungry-delete
        transpose-frame
        try
        # bm
        # counsel-dash
        # hydra
        # keyfreq
        # meghanada
      ])); in pkgs ++ [(runCommand "default.el" {
        inherit rtags ripgrep ag emacs ant nethack fortune gnutls;
        gpg = gnupg1compat;
        jdeeserver = jdee-server;
        aspell = myAspell;
      } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${myConfig.emacs} $out/share/emacs/site-lisp/default.el
          substituteAllInPlace $out/share/emacs/site-lisp/default.el
          # loadPaths=""
          # for f in ${toString pkgs}; do
          #   loadPaths="$loadPaths -L $f/share/emacs/site-lisp/elpa/* -L $f/share/emacs/site-lisp"
          # done
          # $emacs/bin/emacs --batch $loadPaths -f batch-byte-compile "$out/share/emacs/site-lisp/default.el"
        '')]
      );

    userPackages = buildEnv {
      buildInputs = [ makeWrapper ];
      postBuild = ''
        if [ -w $out/share/info ]; then
           shopt -s nullglob
           for i in $out/share/info/*.info $out/share/info/*.info.gz; do # */
             ${texinfoInteractive}/bin/install-info $i $out/share/info/dir
           done
        fi

        mkdir -p $out/etc

        cp ${myConfig.gitconfig} $out/etc/gitconfig
        substituteInPlace $out/etc/gitconfig \
        --replace @gitignore@ ${myConfig.gitignore} \
        --replace @gnupg@ ${gnupg1compat}/bin/gpg \
        --replace @out@ $out

        cp ${myConfig.bashrc} $out/etc/bashrc
        substituteInPlace $out/etc/bashrc \
        --replace @fortune@ ${fortune} \
        --replace @out@ $out

        cp ${myConfig.zshrc} $out/etc/.zshrc
        substituteInPlace $out/etc/.zshrc \
        --replace @zsh-autosuggestions@ ${zsh-autosuggestions} \
        --replace @fortune@ ${fortune} \
        --replace @out@ $out
        cp $out/etc/.zshrc $out/etc/zshrc

        cp ${myConfig.etc-profile} $out/etc/profile
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
            mu
            (runCommand "my-profile" { buildInputs = [makeWrapper]; } ''
              mkdir -p $out/etc/profile.d
              cp ${myConfig.profile} $out/etc/profile.d/my-profile.sh
              substituteInPlace $out/etc/profile.d/my-profile.sh \
                --replace @emacs@ ${myEmacs} \
                --replace @fortune@ ${fortune} \
                --replace @cacert@ ${cacert}
            '')
      ];
    };
  };

}
