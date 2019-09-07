#!/usr/bin/env sh
echo This script will install Nix and Git
echo if they are not already installed.

if ! command -v nix-env >/dev/null 2>&1; then
    nix_installer=$(mktemp)
    curl -s https://nixos.org/nix/install \
      > $nix_installer
    sh $nix_installer
    [ -f $HOME/.profile ] && . $HOME/.profile
fi

if ! command -v git >/dev/null 2>&1 || \
   { [ "$(uname)" = Darwin ] && \
     [ "$(command -v git)" = /usr/bin/git ] &&
     xcode-select -p 1>/dev/null; }; then
    nix-env -iA nixpkgs.git || nix-env -iA nixos.git
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

if ! grep -q 'source $HOME/.nix-profile/etc/profile' $HOME/.profile; then
    echo '[ -f $HOME/.nix-profile/etc/profile ] && source $HOME/.nix-profile/etc/profile' >> $HOME/.profile
fi

if ! grep -q 'source $HOME/.nix-profile/etc/zshrc' $HOME/.zshrc; then
    echo '[ -f $HOME/.nix-profile/etc/zshrc ] && source $HOME/.nix-profile/etc/zshrc' >> $HOME/.zshrc
fi

source $HOME/.nix-profile/etc/profile

echo To use bauer correctly, you must first source the profile.
echo
echo To do this, just run:
echo $ source $HOME/.nix-profile/etc/profile

if [ "$(basename $SHELL)" = zsh ]; then
    source $HOME/.nix-profile/etc/zshrc
    echo $ source $HOME/.nix-profile/etc/zshrc
fi

echo From you command line
echo You can also run either emacs or zsh to launch the environment

if [ -n "$1" ] && \
   command -v git >/dev/null 2>&1 \
   && (echo "$1" | grep -q "^[0-9a-f]\{5,40\}$"); then
    echo Found Gist commit $1, cloning now.
    ./gist-unpack.sh "$1"
fi
