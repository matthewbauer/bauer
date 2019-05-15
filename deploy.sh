#!/usr/bin/env sh
update() {
  ./update.sh
  git add .
  git commit -m "Regen"
}

setup() {
  git stash push
  git checkout gh-pages
}

cleanup() {
  git checkout master
  git stash pop
}

setup
trap cleanup EXIT

git fetch origin
git reset --hard origin/gh-pages
git merge --no-edit master
update
git push origin gh-pages
