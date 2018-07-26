#!/usr/bin/env sh
emacs --batch \
      -l ob-tangle \
      --eval "(org-babel-tangle-file \"README.org\")"
emacs README.org --batch -f org-html-export-to-html
