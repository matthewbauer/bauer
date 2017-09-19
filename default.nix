
{               # nixpkgs-17.09-darwin at 2017-09-10
  nixpkgs-rev ? "b50193f0ca3e9a0d8db2bedfae628995653197d7"
, nixpkgs-url ? "github.com/NixOS/nixpkgs/archive/${nixpkgs-rev}.tar.gz"
, nixpkgs ? builtins.fetchTarball nixpkgs-url
, nixpkgs' ? import nixpkgs {}
}: with nixpkgs';

import (runCommand "README" { buildInputs = [ emacs ]; } ''
mkdir -p $out
cd $out
cp -r ${./lisp} $out/lisp
cp ${./README.org} README.org
emacs --batch --quick \
      -l ob-tangle \
      --eval "(org-babel-tangle-file \"README.org\")"
cp bauer.nix default.nix
'') {inherit nixpkgs';}
