#!/usr/bin/env bash
if [ $# -eq 0 ]; then
    echo Usage: "$0" [ GIST_ID \| URL ] [ -u USER ] [ -t TOKEN ] >&2
    exit 1
fi

FORCE=
GIST_ID=
GH_USER=
TOKEN=
URL=
while [ $# -gt 0 ]; do
    case "$1" in
        -f|--force)
            echo Forcing install... >&2
            FORCE=1
            shift
            ;;
        -u|--user)
            if [ -n "$GH_USER" ]; then
                echo Multiple users passed! >&2
                exit 1
            fi
            shift
            GH_USER="$1"
            shift
            ;;
        -t|--token)
            if [ -n "$TOKEN" ]; then
                echo Multiple tokens passed! >&2
                exit 1
            fi
            shift
            TOKEN="$1"
            shift
            ;;
        -g|--gist-id)
            if [ -n "$GIST_ID" ] || [ -n "$URL" ]; then
                echo Multiple URLs or Gist ids passed! >&2
                exit 1
            fi
            shift
            GIST_ID="$1"
            shift
            ;;
        --url)
            if [ -n "$GIST_ID" ] || [ -n "$URL" ]; then
                echo Multiple URLs or Gist ids passed! >&2
                exit 1
            fi
            shift
            URL="$1"
            shift
            ;;
        https://*|ssh://*|git://*)
             if [ -n "$GIST_ID" ] || [ -n "$URL" ]; then
                echo Multiple URLs or Gist ids passed! >&2
                exit 1
            fi
            URL="$1"
            shift
            ;;
        *)
            if [ -n "$GIST_ID" ] || [ -n "$URL" ]; then
                echo Multiple URLs or Gist ids passed! >&2
                exit 1
            fi
            GIST_ID="$1"
            echo Using gist "$GIST_ID" >&2
            shift
            ;;
    esac
done

if [ -n "$GIST_ID" ]; then
  URL=ssh://git@gist.github.com/"$GIST_ID".git
fi

if [ -z "$URL" ]; then
    echo No gist id or url provided. >&2
    exit 1
fi

mkdir -p $HOME/.ssh
echo "github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl" >> "$HOME/.ssh/known_hosts"
echo "gist.github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl" >> "$HOME/.ssh/known_hosts"

ssh -T git@github.com 2> /dev/null
if [ $? -eq 255 ]; then
    if ! [ -f "$HOME/.ssh/id_ed25519" ]; then
        echo No ssh key is available. Generating it and adding it to GitHub to continue.
        ssh-keygen -t ed25519 -N "" -f "$HOME/.ssh/id_ed25519"
    fi

    if [ -z "$GH_USER" ] && [ -t 1 ]; then
        echo -n "GitHub Username: "; read GH_USER
    fi
    if [ -z "$GH_USER" ]; then
        echo Username cannot be empty. Pass -u with your username to continue.
        exit 1
    fi
    auth="$GH_USER"
    if [ -n "$TOKEN" ]; then
        auth="$auth:$TOKEN"
    fi
    curl -u "$auth" -d "$(printf '{"title": "%s", "key": "%s"}' "${HOST-$(hostname)}" "$(cat $HOME/.ssh/id_ed25519.pub)")" https://api.github.com/user/keys > /dev/null
fi

gistdir="$(mktemp -d)"
setup() {
    if ! git clone "$URL" "$gistdir"; then
        echo Failed to clone Gist. Verify "$URL" exists >&2
        echo and you have permission to access it. >&2
        exit 1
    fi

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
shopt -s nullglob
for f in *; do
    if [ "$f" = ".git" ]; then
        continue
    fi
    if [ "$f" = gnu.emacs.daemon.plist ] && [ "$(uname)" != Darwin ]; then
        continue
    fi
    if ! [ -f "$f" ]; then
        echo Skipping "$f", not a file >&2
        continue
    fi
    DEST=
    case "$f" in
        settings.el) DEST="$HOME/.config/emacs/settings.el" ;;
        .sshconfig) DEST="$HOME/.ssh/config" ;;
        .ssh_authorized_keys) DEST="$HOME/.ssh/authorized_keys" ;;
        nix.conf) DEST="$HOME/.config/nix/nix.conf" ;;
        machines) DEST="$HOME/.config/nix/machines" ;;
        .gitignore) DEST="$HOME/.config/git/ignore" ;;
        .gitconfig) DEST="$HOME/.config/git/config" ;;
        .gitattributes) DEST="$HOME/.config/git/attributes" ;;
        credentials|.aws_credentials) DEST="$HOME/.aws/credentials" ;;
        gnu.emacs.daemon.plist) DEST="$HOME/Library/LaunchAgents/gnu.emacs.daemon.plist" ;;
        *) DEST="$HOME/$f" ;;
    esac
    CONCAT=
    case "$f" in
        .authinfo) CONCAT=1 ;;
        .sshconfig) CONCAT=1 ;;
        .ssh_authorized_keys) CONCAT=1 ;;
        .gitignore) CONCAT=1 ;;
        nix.conf) CONCAT=1 ;;
        machines) CONCAT=1 ;;
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
    case "$f" in
         .authinfo) chmod 600 "$DEST" ;;
         gnu.emacs.daemon.plist) launchctl load -w "$DEST" ;;
    esac
done
