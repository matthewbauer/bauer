#!/usr/bin/env sh
if [ $# -eq 0 ]; then
    echo Usage: "$0" GIST_ID >&2
    exit 1
fi

FORCE=
GIST_ID=
while [ $# -gt 0 ]; do
    case "$1" in
        -f|--force)
            echo Forcing install... >&2
            FORCE=1
            shift
            ;;
        *)
            if [ -n "${GIST_ID-}" ]; then
                echo Multiple Gist ids passed! >&2
                exit 1
            fi
            GIST_ID="$1"
            echo Using gist "$GIST_ID"x >&2
            shift
            ;;
    esac
done

if [ -z "$GIST_ID" ]; then
    echo No gist id provided. >&2
    exit 1
fi

gistdir="$(mktemp -d)"
setup() {
    git clone git@github.com:"$GIST_ID".git "$gistdir"
    pushd "$gistdir" >/dev/null
}

cleanup() {
    popd >/dev/null
    rm -rf "$gistdir"
}

setup
trap cleanup EXIT

if [ -n "${BASH_VERSION-}" ]; then
    shopt -s dotglob
fi
for f in *; do
    if [ "$f" = ".git" ]; then
        continue
    fi
    if ! [ -f "$f" ]; then
        echo Skipping "$f", not a file >&2
        continue
    fi
    DEST=
    case "$f" in
        settings.el) DEST="$HOME/.emacs.d/settings.el" ;;
        .sshconfig) DEST="$HOME/.ssh/config" ;;
        .ssh_authorized_keys) DEST="$HOME/.ssh/authorized_keys" ;;
        nix.conf) DEST="$HOME/.config/nix/nix.conf" ;;
        .gitignore) DEST="$HOME/.config/git/ignore" ;;
        .gitconfig) DEST="$HOME/.config/git/config" ;;
        *) DEST="$HOME/$f" ;;
    esac
    CONCAT=
    case "$f" in
        .authinfo) CONCAT=1 ;;
        .sshconfig) CONCAT=1 ;;
        .ssh_authorized_keys) CONCAT=1 ;;
        .gitignore) CONCAT=1 ;;
        nix.conf) CONCAT=1 ;;
    esac
    if [ -z "$DEST" ]; then
        echo Skipping "$f", no destination found >&2
        continue
    fi
    if [ -f "$DEST" ] && [ -z "$FORCE" ] && [ -z "$CONCAT" ]; then
        echo Skipping "$f", destination already exists >&2
        continue
    fi
    mkdir -p "$(dirname "$DEST")"
    if [ -n "$CONCAT" ]; then
        cat "$f" >> "$DEST"
    else
        cp "$f" "$DEST"
    fi
    if [ "$f" = .authinfo ]; then
        chmod 600 "$DEST"
    fi
done
