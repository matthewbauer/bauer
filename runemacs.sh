#!/usr/bin/env sh

case $(uname) in
    Darwin)
        open @out@/Applications/Emacs.app
    ;;
    *)
        @out@/bin/emacs
    ;;
esac
