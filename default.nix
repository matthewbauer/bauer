# -*- mode: nix; coding: utf-8; -*-
{ channel ? "nixpkgs-unstable"
, nixpkgs-url ?
  "nixos.org/channels/${channel}/nixexprs.tar.xz"
, nixpkgs ? import (builtins.fetchTarball nixpkgs-url) {}
}: with nixpkgs;

let
ensure = f: n: if builtins.pathExists f then f
               else builtins.fetchurl
               "https://matthewbauer.us/bauer/${n}";
in import (runCommand "README" {
  buildInputs = [ emacs git ];
} (''
  install -D ${ensure ./README.org "README.org"} \
          $out/README.org
  cd $out
'' + lib.optionalString (builtins.pathExists ./site-lisp) ''
  cp -r ${./site-lisp} site-lisp
'' + ''
  emacs --batch --quick \
	-l ob-tangle \
	--eval "(org-babel-tangle-file \"README.org\")"
  cp bauer.nix default.nix
'')) { inherit nixpkgs ensure; }
