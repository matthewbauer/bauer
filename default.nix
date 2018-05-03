{ channel ? (if builtins.currentSystem == "x86_64-darwin"
             then "nixpkgs-unstable"
             else "nixos-unstable")
, nixpkgs-url ? "nixos.org/channels/${channel}/nixexprs.tar.xz"
, nixpkgs ? import (builtins.fetchTarball nixpkgs-url) {}
}: with nixpkgs;

import (runCommand "README" { buildInputs = [ emacs git ]; } (''
  install -D ${./README.org} $out/README.org
  cd $out
'' + lib.optionalString (builtins.pathExists ./site-lisp) ''
  cp -r ${./site-lisp} site-lisp
'' + ''
  emacs --batch --quick \
	-l ob-tangle \
	--eval "(org-babel-tangle-file \"README.org\")"
  cp bauer.nix default.nix
'')) { inherit nixpkgs; }
