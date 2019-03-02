# [[file:~/.local/share/bauer/README.org::*NixOS%20module][NixOS module:2]]
# -*- mode: nix; coding: utf-8; -*-
{ config, lib, pkgs, ... }: with lib;

let
  ensure = f: n: if builtins.pathExists f then f
                 else builtins.fetchurl
    "https://matthewbauer.us/bauer/${n}";
  bauer = import (ensure ./default.nix "default.nix") {
    inherit pkgs;
  };
in {
  options = {
    programs.bauer = {
      enable = mkOption {
        default = false;
        type = types.bool;
      };
    };
  };

  config = mkIf config.programs.bauer.enable {
    environment = {
      systemPackages = [ bauer ];
      variables = with lib; {
        PATH = [bauer.PATH];
        XDG_DATA_DIRS = [bauer.XDG_DATA_DIRS];
        MANPATH = [bauer.MANPATH];
        INFOPATH = [bauer.INFOPATH];
      };
    };
  };
}
# NixOS module:2 ends here
