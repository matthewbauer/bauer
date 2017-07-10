{
  packageOverrides = pkgs: with pkgs; rec {
    nix = nixStable;

    rls = rustPlatform.buildRustPackage {
      name = "rls";

      src = fetchFromGitHub {
        owner = "rust-lang-nursery";
        repo = "rls";
        rev = "931aba8d0ed2f84972acaada56b998babfb021a0";
        sha256 = "1s5pffp45a6acnxhkb9k4pxb584nfbnd7qhaqckxng21i5bj9s9r";
      };

      buildInputs = [ openssl ];

      depsSha256 = "0r4im0dcmil6zazbbc85xpxjv0fh0m6kpr1scjqyhqw11wdpv59c";
    };

    pls = pythonPackages.buildPythonPackage {
      name = "pls";
      src = fetchFromGitHub {
        owner = "palantir";
        repo = "python-language-server";
        rev = "0.2.2";
        sha256 = "1z8psnyzpfcfgz5ysnd50m14qr78kinl4vl6y54xv1ljgspz4xvs";
      };
    };

    hls = haskell.lib.buildStackProject {
      name = "hls";
      buildInputs = [ ];
      src = fetchFromGitHub {
        owner = "alanz";
        repo = "haskell-lsp";
        rev = "ece7ab737810e924749be8bb034fea5243e74063";
        sha256 = "0dkwrn0sv41dn4ap1yagm52c7ww283fxnsycbz8n0d2whj77dj7m";
      };
    };

    jls = stdenv.mkDerivation {
      name = "jls";
      src = fetchurl {
        url = "http://download.eclipse.org/jdtls/snapshots/jdt-language-server-0.2.0-201707061822.tar.gz";
        sha256 = "05q65658ysrw4xcn5fhlp5xfqp70kwkzngdkhhw7vpxx18hb4dfd";
      };
      sourceRoot = ".";
      installPhase = ''
        mkdir $out
        cp -r config_* features plugins $out
      '';
    };

    gls = callPackage ./gls {};

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

    myEmacs = let
      epkgs''' = runCommand "packages-list" { buildInputs = [ emacs ]; } ''
        emacs -batch \
          -L ${customEmacsPackages.melpaStablePackages.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -L ${customEmacsPackages.melpaStablePackages.use-package}/share/emacs/site-lisp/elpa/use-package-* \
          -l ${./get-packages.el} \
          --eval "(get-packages \"${./default.el}\")))" > $out
      '';
      epkgs' = builtins.fromJSON (builtins.readFile epkgs''');
    in customEmacsPackages.emacsWithPackages (epkgs: let
        epkgs'' = map (x: if x == "rtags" then pkgs.rtags
              else if x == "nix-mode" then pkgs.nix
              else if x == "ghc" then pkgs.ghc
              else if x == "notmuch" then pkgs.notmuch
              else if builtins.hasAttr x epkgs.elpaPackages then builtins.getAttr x epkgs.elpaPackages
              # else if builtins.hasAttr x epkgs.melpaStablePackages then builtins.getAttr x epkgs.melpaStablePackages
              else if builtins.hasAttr x epkgs.melpaPackages then builtins.getAttr x epkgs.melpaPackages
              else if builtins.hasAttr x epkgs then builtins.getAttr x epkgs
              else if builtins.hasAttr x emacsPackages then builtins.getAttr x emacsPackages
              else builtins.getAttr x pkgs) epkgs';
      in epkgs'' ++ [epkgs.melpaStablePackages.use-package
      (runCommand "default.el" {
        inherit rtags ripgrep ag emacs ant nethack fortune gnutls;
        gpg = gnupg1compat;
        jdeeserver = jdee-server;
        aspell = myAspell;
      } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${./default.el} $out/share/emacs/site-lisp/default.el
          cp ${./eshell-extras.el} $out/share/emacs/site-lisp/eshell-extras.el
          substituteAllInPlace $out/share/emacs/site-lisp/default.el
        '')]);

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
        ctags
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
