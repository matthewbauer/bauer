
#!/bin/sh
out=$1

if [ -z "$out" ]; then
  out=$(nix-build)
fi

echo Store size:
du -scl $out

echo Dependencies:
du -scl $(nix-store -qR $out) | sort -n
