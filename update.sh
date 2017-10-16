#!/bin/sh
emacs --batch \
      -l ob-tangle --eval "(org-babel-tangle-file \"README.org\")"
