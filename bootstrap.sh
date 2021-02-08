#!/usr/bin/env sh
{

set -e

echo This script will install Nix and Git
echo if they are not already installed.

if ! command -v nix-env >/dev/null 2>&1; then
    case "$(uname)" in
      Darwin) curl -L -s https://nixos.org/nix/install | sh -s - --darwin-use-unencrypted-nix-store-volume --daemon ;;
      Linux) curl -L -s https://nixos.org/nix/install | sh -s - --daemon ;;
      *) curl -L -s https://nixos.org/nix/install | sh -s ;;
    esac
    [ -f $HOME/.profile ] && . $HOME/.profile
    PATH="$HOME/.nix-profile/bin${PATH-+:$PATH}" # in case above didn't work
fi

if ! command -v git >/dev/null 2>&1 || \
   { [ "$(uname)" = Darwin ] && \
     [ "$(command -v git)" = /usr/bin/git ] &&
     ! xcode-select -p >/dev/null 2>&1; }; then
    nix-env -iA nixpkgs.git 2>/dev/null || nix-env -iA nixos.git || nix --experimental-features 'nix-command flakes' profile install nixpkgs#git || nix-env -iA git -f https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz
fi

if ! command -v ssh >/dev/null 2>&1; then
    nix-env -iA nixpkgs.openssh 2>/dev/null || nix-env -iA nixos.openssh || nix --experimental-features 'nix-command flakes' profile install nixpkgs#openssh || nix-env -iA openssh -f https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz
fi

if [ -d .git ]; then
    git pull origin master || true
fi

if ! [ -f default.nix ]; then
    repo_dir=$HOME/.local/share/bauer
    mkdir -p $(dirname $repo_dir)
    if ! [ -d $repo_dir/.git ]; then
      git clone https://github.com/matthewbauer/bauer $repo_dir
    else
      git -C $repo_dir pull
    fi
    cd $repo_dir
fi

if [ -z "${GIST_ID-}" ] && [ -n "$1" ] && (echo "$1" | grep -q "^[0-9a-f]\{5,40\}$"); then
  GIST_ID="$1"
fi

if [ -n "${GIST_ID-}" ]; then
  URL=ssh://git@github.com/"$GIST_ID".git
fi

if [ -z "${URL-}" ]; then
  case "${USER-}" in
    mbauer|matthewbauer) URL=ssh://git@github.com/matthewbauer/dotfiles.git ;;
  esac
fi

if [ -n "${URL-}" ]; then
  echo Found Gist commit "$1", cloning now.
  ./gist-unpack.sh "$URL"
fi

nix-env -if . || nix --experimental-features 'nix-command flakes' profile install

if ! [ -f "$HOME/.profile" ] || ! grep -q '\(source\|\.\) "\?$HOME/.nix-profile/etc/profile"\?' "$HOME/.profile"; then
    echo '[ -f "$HOME/.nix-profile/etc/profile" ] && . "$HOME/.nix-profile/etc/profile"' >> "$HOME/.profile"
fi

if ! [ -f "$HOME/.zshenv" ] || ! grep -q '\(source\|\.\) "\?$HOME/.nix-profile/etc/zshrc"\?' "$HOME/.zshenv"; then
    echo '[ -f "$HOME/.nix-profile/etc/zshrc" ] && source "$HOME/.nix-profile/etc/zshrc"' >> "$HOME/.zshenv"
fi

if ! [ -f "$HOME/.bashrc" ] || ! grep -q '\(source\|\.\) "\?$HOME/.nix-profile/etc/profile"\?' "$HOME/.bashrc"; then
    echo '[ -f "$HOME/.nix-profile/etc/profile" ] && source "$HOME/.nix-profile/etc/profile"' >> "$HOME/.bashrc"
fi

echo To use bauer correctly, you must first source the profile.
echo
echo To do this, just run:
echo $ . $HOME/.nix-profile/etc/profile

if [ -n "${ZSH_NAME-}" ]; then
    echo $ . $HOME/.nix-profile/etc/zshrc
fi

echo From you command line
echo You can also run either emacs or zsh to launch the environment

}
