#!/usr/bin/env bash
if [ $# -eq 0 ]; then
    echo Usage: "$0" GIST_ID [ -u USER ] [ -t TOKEN ] >&2
    exit 1
fi

FORCE=
GIST_ID=
USER=
TOKEN=
while [ $# -gt 0 ]; do
    case "$1" in
        -f|--force)
            echo Forcing install... >&2
            FORCE=1
            shift
            ;;
        -u|--user)
            if [ -n "$USER" ]; then
                echo Multiple users passed! >&2
                exit 1
            fi
            shift
            USER="$1"
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
            if [ -n "$GIST_ID" ]; then
                echo Multiple Gist ids passed! >&2
                exit 1
            fi
            shift
            GIST_ID="$1"
            shift
            ;;
        *)
            if [ -n "$GIST_ID" ]; then
                echo Multiple Gist ids passed! >&2
                exit 1
            fi
            GIST_ID="$1"
            echo Using gist "$GIST_ID" >&2
            shift
            ;;
    esac
done

if [ -z "$GIST_ID" ]; then
    echo No gist id provided. >&2
    exit 1
fi

mkdir -p $HOME/.ssh
echo "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==" >> $HOME/.ssh/known_hosts

ssh -T git@github.com 2> /dev/null
if [ $? -eq 255 ]; then
    if ! [ -f "$HOME/.ssh/id_rsa" ]; then
        echo No ssh key is available. Generating it and adding it to GitHub to continue.
        ssh-keygen -t rsa -N "" -f "$HOME/.ssh/id_rsa"
    fi

    if [ -z "$USER" ] && [ -t 1 ]; then
        echo -n "GitHub Username: "; read USER
    fi
    if [ -z "$USER" ]; then
        echo Username cannot be empty. Pass -u with your username to continue.
        exit 1
    fi
    auth="$USER"
    if [ -n "$TOKEN" ]; then
        auth="$auth:$TOKEN"
    fi
    curl -u "$auth" -d "$(printf '{"title": "%s", "key": "%s"}' "${HOST-$(hostname)}" "$(cat $HOME/.ssh/id_rsa.pub)")" https://api.github.com/user/keys > /dev/null
fi

gistdir="$(mktemp -d)"
setup() {
    if ! git clone git@github.com:"$GIST_ID".git "$gistdir"; then
        echo Failed to clone Gist. Verify "https://gist.github.com/$GIST_ID" exists >&2
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
        .gitignore) DEST="$HOME/.config/git/ignore" ;;
        .gitconfig) DEST="$HOME/.config/git/config" ;;
        .gitattributes) DEST="$HOME/.config/git/attributes" ;;
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
