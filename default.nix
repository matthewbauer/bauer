{nixpkgs ? import <nixpkgs> {}}:

with nixpkgs;

let config = import ./config.nix;
    pkgs = pkgs // config.packageOverrides;
in
    userPackages
