# -*- mode: nix; coding: utf-8; -*-
{ version ? "21.11"
, channel ? {
    "x86_64-darwin" = "nixpkgs-unstable";
  }.${builtins.currentSystem} or "nixos-${version}"
, nixpkgs-url ?
  "https://nixos.org/channels/${channel}/nixexprs.tar.xz"
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, overlays ? []
, pkgs ? import (builtins.fetchTarball nixpkgs-url) {
  inherit crossSystem system config overlays;
}
, small ? true
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
        --eval "(org-babel-tangle-file \"README.org\")" > /dev/null
  cp bauer.nix default.nix
'')) { inherit ensure pkgs small system; }
