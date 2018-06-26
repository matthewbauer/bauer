# -*- mode: nix; coding: utf-8; -*-
{ channel ? "nixpkgs-unstable"
, nixpkgs-url ?
  "nixos.org/channels/${channel}/nixexprs.tar.xz"
, pkgs ? import (builtins.fetchTarball nixpkgs-url) {}
, small ? false
, ...
} @ args:

let
ensure = f: n: if builtins.pathExists f then f
	       else builtins.fetchurl
	       "https://matthewbauer.us/bauer/${n}";
in import (pkgs.runCommand "README" {
  buildInputs = with pkgs; [ emacs git ];
} (''
  install -D ${ensure ./README.org "README.org"} \
	  $out/README.org
  cd $out
'' + pkgs.lib.optionalString (builtins.pathExists ./site-lisp) ''
  cp -r ${./site-lisp} site-lisp
'' + ''
  emacs --batch --quick \
	-l ob-tangle \
  --eval "(org-babel-tangle-file \"README.org\")"
  cp bauer.nix default.nix
'')) (args // { inherit ensure pkgs; })
