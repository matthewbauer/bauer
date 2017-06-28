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
      ]
      ++ (with emacsPackages; [
        proofgeneral
        ess
      ])
      ++ (with epkgs.elpaPackages; [
        org
        tiny
        rainbow-mode
        realgud
        dired-du
        all
        easy-kill
        excorporate
        ggtags
        gnorb
        gnugo
        chess
        muse
        w3
        windresize
        vlf

        ace-window
        aggressive-indent
        auctex
        avy
        bug-hunter
        coffee-mode
        company
        docbook
        electric-spacing
        ivy
        js2-mode
        json-mode
        minimap
        other-frame-window
        python
        undo-tree
      ])
      ++ (with epkgs.melpaStablePackages; [
        ace-jump-mode
        ag
        bind-key
        buffer-move
        counsel
        diminish
        diffview
        dumb-jump
        esup
        expand-region
        flx
        shut-up
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
        multi-line
        multiple-cursors
        mmm-mode
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
      ])
      ++ (with epkgs.melpaPackages; [
        autodisass-java-bytecode
        smart-hungry-delete
        # meghanada
        apropospriate-theme
        c-eldoc
        css-eldoc
        go-eldoc
        company-flx
        counsel-projectile
        indium
        jdee
        esh-help
        eshell-prompt-extras
        kill-or-bury-alive
        transpose-frame
        mediawiki
        rg
        noflet
        elpy
        hydra
        bm
        diff-hl
        hideshowvis
        try
        counsel-dash
        irony
        company-irony
        flycheck-irony
        dtrt-indent
      ])); in pkgs ++ [(runCommand "default.el" { inherit rtags ripgrep ag emacs ant nethack fortune; gpg = gnupg1compat; jdeeserver = jdee-server; aspell = myAspell; } ''
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
        name = "user-packages";
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
            offlineimap
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
