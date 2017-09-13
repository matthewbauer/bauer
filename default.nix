let # nixpkgs-17.09-darwin at 2017-09-10
    nixpkgsRev = "b50193f0ca3e9a0d8db2bedfae628995653197d7";
    defaultNixpkgs = builtins.fetchTarball
      "github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
in { nixpkgs ? defaultNixpkgs, nixpkgs_ ? import nixpkgs {} }:
with nixpkgs_;

import (stdenv.mkDerivation {
         name = "README";
         unpackPhase = "true";
         buildInputs = [ emacs ];
         installPhase = ''
mkdir -p $out
cd $out
cp ${./README.org} README.org
cp -r ${./lisp} $out/lisp
emacs --batch -l ob-tangle --eval "(org-babel-tangle-file \"README.org\")"
cp bauer.nix default.nix
         '';
}) {}
