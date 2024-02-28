{
  description = "an Emacs+Nix IDE";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, emacs-overlay }: let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
  in {
    packages = forAllSystems (system: {
      bauer = let
        pkgs = nixpkgsFor.${system};
        evalPkgs = nixpkgsFor.${system};
      in import (evalPkgs.runCommand "README" {
        buildInputs = with evalPkgs; [ pkgs.emacs git ];
      } (''
        install -D ${./README.org} $out/README.org
        cd $out
        cp -r ${./site-lisp} site-lisp
        emacs --batch --quick \
              -l ob-tangle \
              --eval "(org-babel-tangle-file \"README.org\")" > /dev/null
        cp bauer.nix default.nix
      '')) { inherit pkgs emacs-overlay evalPkgs system; };
      inherit (self.packages.${system}.bauer) myTex emacs rEnv;
      default = self.packages.${system}.bauer;
    });

    apps = forAllSystems (system: {
      bauer = {
        type = "app";
        program = "${self.packages.${system}.bauer}/bin/run";
      };
      default = self.apps.${system}.bauer;
    });

    devShells = forAllSystems (system: {
      bauer = with nixpkgsFor.${system}; stdenv.mkDerivation {
        name = "bauer";
        nativeBuildInputs = [ pkgs.emacs git ];
        shellHook = ''
          echo Run ./update.sh to generate files.
        '';
      };
      default = self.devShells.${system}.bauer;
    });

    nixosModules.bauer = {
      imports = [ ./module.nix ];
    };
  };

  nixConfig.allow-import-from-derivation = true;
}
