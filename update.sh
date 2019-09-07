#!/usr/bin/env sh
emacs --batch \
      -l ob-tangle \
      --eval "(org-babel-tangle-file \"README.org\")"
emacs README.org --batch \
  --eval "(setq org-html-htmlize-output-type 'css)" \
  -l nix-mode \
  -f org-html-export-to-html
