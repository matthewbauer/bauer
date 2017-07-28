{
  packageOverrides = pkgs: with pkgs; rec {
    nix = nixStable;

    rls = let mozpkgs = (import (fetchFromGitHub {
        owner = "mozilla";
        repo = "nixpkgs-mozilla";
        rev = "26c8cccaeb152db32f02a97e055ff58df649cd78";
        sha256 = "0y7wfz0nh59k19kc5qk6w822yds2wi18bcngzypwh4b0n6dw0szx";
      }) {});
      rustNightlyNixRepo = pkgs.fetchFromGitHub {
        owner = "solson";
        repo = "rust-nightly-nix";
        rev = "9e09d579431940367c1f6de9463944eef66de1d4";
        sha256 = "03zkjnzd13142yla52aqmgbbnmws7q8kn1l5nqaly22j31f125xy";
      };
      rustPackages = pkgs.callPackage rustNightlyNixRepo { };
      rustPlatform = makeRustPlatform rustBeta;
      # rustPlatform = rustNightlyBin;
    in callPackage ./rls {
      # inherit rustPlatform;
      rustPlatform = makeRustPlatform rustNightly;
    };
    python-language-server = callPackage ./python-language-server {};
    haskell-lsp = haskellPackages.callPackage ./haskell-lsp {};
    jdt-language-server = callPackage ./jdt-language-server;
    go-langserver = callPackage ./go-langserver {};

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
      buildPhase = ''
        mvn -DskipTests=true -Dmaven.repo.local=$(pwd) assembly:assembly
      '';
      installPhase = "mkdir $out; cp target/jdee-bundle-*.jar $out";
    };

    myEmacs = let
      epkgs''' = runCommand "packages-list" { buildInputs = [ emacs ]; } ''
        emacs -batch \
          -L ${customEmacsPackages.melpaStablePackages.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -L ${customEmacsPackages.melpaStablePackages.use-package}/share/emacs/site-lisp/elpa/use-package-* \
          -l ${./use-package-list.el} \
          --eval "(use-package-list \"${default}/share/emacs/site-lisp/default.el\")))" > $out
      '';
      epkgs' = builtins.fromJSON (builtins.readFile epkgs''');
      default = (runCommand "default.el" {
        inherit rtags ripgrep ag emacs ant nethack fortune gnutls
                coreutils findutils openssh git bash
                zsh perl golint perlcritic
                go asciidoc lessc stack
                lua gcc; # csslint
        inherit (pythonPackages) flake8;
        inherit (nodePackages) jshint;
        tidy = html-tidy;
        jsonlint = pythonPackages.demjson;
        libxml2 = libxml2.bin;
        gpg = gnupg1compat;
        jdeeserver = jdee-server;
        aspell = myAspell;
      } ''
          mkdir -p $out/share/emacs/site-lisp
          cp ${./default.el} $out/share/emacs/site-lisp/default.el
          cp ${./em-dired.el} $out/share/emacs/site-lisp/em-dired.el
          cp ${./dired-column.el} $out/share/emacs/site-lisp/dired-column.el
          cp ${./macho-mode.el} $out/share/emacs/site-lisp/macho-mode.el
          substituteAllInPlace $out/share/emacs/site-lisp/default.el
        '');
    in customEmacsPackages.emacsWithPackages (epkgs: let
        epkgs'' = map (x: if x == "rtags" then pkgs.rtags
              else if builtins.hasAttr x epkgs.elpaPackages
                then builtins.getAttr x epkgs.elpaPackages
              else if builtins.hasAttr x epkgs.melpaPackages
                then builtins.getAttr x epkgs.melpaPackages
              else if builtins.hasAttr x epkgs then builtins.getAttr x epkgs
              else if builtins.hasAttr x emacsPackages
                 then builtins.getAttr x emacsPackages
              else builtins.getAttr x pkgs) epkgs';
      in epkgs'' ++ [epkgs.melpaStablePackages.use-package default]);

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
        # bash-completion
        # zsh-completions
        # myAspell
        myEmacs
        # gcc
        # gawk
        bashInteractive
        # bc
        # bzip2
        # cabal-install
        # cabal2nix
        # cargo
        # checkbashisms
        # cmake
        coreutils
        # clang
        # diffutils
        # editorconfig-core-c
        # emscripten
        # ffmpeg
        # findutils
        # ripgrep
        # ag
        # ghc
        git
        # gitAndTools.hub
        # go2nix
        # gnugrep
        # gnumake
        # gnuplot
        # gnused
        gnupg1compat
        # gnutar
        # gnutls
        # go
        # gzip
        # jdk
        # jq
        # haskellPackages.intero
        # lua
        # less
        # man
        # nano
        # nasm
        nox
        nix
        # nix-prefetch-scripts
        nix-index
        # nix-repl
        # nix-zsh-completions
        # ninja
        # rtags
        # nmap
        # nodePackages.tern
        # nodejs
        # openssh
        # openssl
        # pandoc
        # patch
        # pypi2nix
        # python
        # perl
        # php
        # pwgen
        # rsync
        # ruby
        # rustc
        # screen
        # stack
        # time
        # tree
        # unzip
        # vim
        # which
        # w3m
        # wget
        # v8
        # xz
        # zip
        zsh
        # fortune
        rEnv
        isync
        # ctags
        # notmuch
        (texlive.combine { inherit (texlive) scheme-medium xetex setspace fontspec chktex; })
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
