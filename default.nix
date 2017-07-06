{nixpkgs ? import <nixpkgs> {}}:

with nixpkgs;

let config = import ./config.nix;
    pkgs_ = config.packageOverrides pkgs;
in
  pkgs_.bauer
