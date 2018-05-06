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
      variables = {
        PATH = [ "${bauer.bins}/bin" ];
        XDG_DATA_DIRS = [ "${bauer.xdg-data}/share" ];
        MANPATH = [ "${bauer.manpages}/share/man" ];
        INFOPATH = [ "${bauer.infopages}/share/info" ];
      };
    };
  };
}
