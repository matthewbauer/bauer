# [[file:~/.local/share/bauer/README.org::*NixOS%20virtualbox%20&%20installer][NixOS virtualbox & installer:1]]
# -*- mode: nix; coding: utf-8; -*-
{ system ? builtins.currentSystem }:
let
  ensure = f: n: if builtins.pathExists f then f
                 else builtins.fetchurl
    "https://matthewbauer.us/bauer/${n}";
in {
  ova = (import <nixpkgs/nixos/lib/eval-config.nix> {
    inherit system;
    modules = [
      <nixpkgs/nixos/modules/installer/virtualbox-demo.nix>
      (ensure ./module.nix "module.nix")
    ];
  }).config.system.build.virtualBoxOVA;
  iso = (import <nixpkgs/nixos/lib/eval-config.nix> {
    inherit system;
    modules = [
      <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-kde.nix>
      (ensure ./module.nix "module.nix")
    ];
  }).config.system.build.isoImage;
}
# NixOS virtualbox & installer:1 ends here
