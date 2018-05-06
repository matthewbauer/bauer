{ config, lib, pkgs, ... }:

with lib;

let
  bauer = import ./bauer.nix {
    nixpkgs = pkgs;
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
      variables = {
        PATH = [ "${bauer.bins}/bin" ];
        XDG_DATA_DIRS = [ "${bauer.xdg-data}/share" ];
        MANPATH = [ "${bauer.manpages}/share/man" ];
        INFOPATH = [ "${bauer.infopages}/share/info" ];
      };
    };
  };
}
