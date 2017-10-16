#!/bin/sh
out=$1

if [ -z "$out" ]; then
  out=$(nix-build)
fi

if [ -L "$out" ]; then
  out=$(readlink -f $out)
fi

if [ -e $out ]; then
  echo Dependencies:
  du -scl $(nix-store -qR $out) | sort -n
fi
