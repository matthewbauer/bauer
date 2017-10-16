{ nixpkgs-url ? "nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz"
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
