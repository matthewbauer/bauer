#!/bin/sh

if ! command -v nix-env >/dev/null 2>&1; then
    nix_installer=$(mktemp)
    curl -s https://nixos.org/nix/install > $nix_installer
    sh $nix_installer
fi

if ! [ -f default.nix ]; then
    repo_dir=$(mktemp -d)
    git clone https://github.com/matthewbauer/nixpkgs-config $repo_dir
    cd $repo_dir
fi

# nixpkgs-17.09-darwin at 2017-09-10
# nixpkgsRev=2b1521bfc754df4a2dc8ecd8581178d22988a04c
# NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/$nixpkgsRev.tar.gz

nix-env -if.
