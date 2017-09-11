let
  # channel = "nixpkgs-17.09-darwin";
  # defaultNixpkgs = builtins.fetchTarball
  #   "github.com/NixOS/nixpkgs-channels/archive/${channel}.tar.gz";

  # nixpkgs-17.09-darwin at 2017-09-10
  nixpkgsRev = "b50193f0ca3e9a0d8db2bedfae628995653197d7";
  defaultNixpkgs = builtins.fetchTarball
    "github.com/NixOS/nixpkgs/archive/${nixpkgsRev}.tar.gz";
in

{ nixpkgs ? defaultNixpkgs }:

with (import nixpkgs {});

let config = import ./config.nix;
    pkgs_ = config.packageOverrides pkgs;
in
  pkgs_.bauer
