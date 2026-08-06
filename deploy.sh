#!/usr/bin/env sh
set -e

# Only pop a stash we created ourselves: an unconditional push/pop pair
# would pop a pre-existing, unrelated stash when the tree was clean.
STASHED=0

setup() {
  if ! git diff-index --quiet HEAD --; then
    git stash push
    STASHED=1
  fi
  git checkout gh-pages
}

cleanup() {
  git checkout master
  if [ "$STASHED" = 1 ]; then
    git stash pop
  fi
}

trap cleanup EXIT
setup

git fetch origin
git reset --hard origin/gh-pages
git merge --no-edit master
./update.sh
git add .
git commit -m "Regen"
git push origin gh-pages
