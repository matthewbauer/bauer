#!/usr/bin/env sh
echo This script will install Nix and Git
echo if they are not already installed.

if ! command -v nix-env >/dev/null 2>&1; then
    nix_installer=$(mktemp)
    curl -s https://nixos.org/nix/install \
      > $nix_installer
    sh $nix_installer
    source $HOME/.profile
fi

if ! command -v git >/dev/null 2>&1; then
    nix-env -iA nixpkgs.git
fi

if [ -d .git ]; then
    git pull origin master || true
fi

if ! [ -f default.nix ]; then
    repo_dir=$HOME/.local/share/bauer
    mkdir -p $(dirname $repo_dir)
    git clone https://github.com/matthewbauer/bauer \
              $repo_dir
    cd $repo_dir
fi

nix-env -if .

echo "source $HOME/.nix-profile/etc/profile" >> $HOME/.profile

echo "To use bauer correctly, you must first source the profile."
echo
echo "To do this, just run:"
echo "  source $HOME/.nix-profile/etc/profile"
echo "From you command line"
echo "You can also run either emacs or zsh to launch the environment"
