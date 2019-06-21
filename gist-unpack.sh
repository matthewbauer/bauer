#!/usr/bin/env sh

# Private file handling for new environments. Unpack private files
# from a Gist into your environment. Must have already setup SSH
# GitHub authentication.

# To set up GitHub SSH run:
# $ ssh-keygen

# Then, add $HOME/.ssh/id_rsa.pub through GitHub’s Web UI.

if [ $# -eq 0 ]; then
    echo "Usage: $0 GIST_ID" >&2
    exit 1
fi

FORCE=
GIST_ID=
while [ $# -gt 0 ]; do
    case "$1" in
	--force)
	    echo "Forcing install..." >&2
	    FORCE=1
	    shift
	    ;;
	*)
	    if [ -n "$GIST_ID" ]; then
		echo "Multiple Gist ids passed!" >&2
	    fi
	    GIST_ID=$1
	    echo "Using gist $GIST_ID" >&2
	    shift
	    ;;
    esac
done

gistdir=$(mktemp -d)
setup() {
    git clone git@github.com:$GIST_ID.git $gistdir
    pushd $gistdir >/dev/null
}

cleanup() {
    popd >/dev/null
    rm -rf $gistdir
}

setup
trap cleanup EXIT

shopt -s dotglob
for f in *; do
    if [ "$f" = ".git" ]; then
	continue
    fi
    if ! [ -f "$f" ]; then
	echo "Skipping $f, not a file" >&2
	continue
    fi
    DEST=
    case "$f" in
	settings.el) DEST="$HOME/.emacs.d/settings.el" ;;
	*) DEST="$HOME/$f" ;;
    esac
    if [ -z "$DEST" ]; then
	echo "Skipping $f, no destination found" >&2
	continue
    fi
    if [ -f "$DEST" ] && [ -z "$FORCE" ]; then
	echo "Skipping $f, destination already exists" >&2
	continue
    fi
    cp "$f" "$DEST"
done
