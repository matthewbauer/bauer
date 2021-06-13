{
  description = "an Emacs+Nix IDE";

  inputs.nixpkgs.url = "github:matthewbauer/nixpkgs?ref=aarch64-darwin-emacs";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  outputs = { self, nixpkgs, emacs-overlay }: let
    systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
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
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.bauer);

    apps = forAllSystems (system: {
      bauer = {
        type = "app";
        program = "${self.packages.${system}.bauer}/bin/run";
      };
    });

    defaultApp = forAllSystems (system: self.apps.${system}.bauer);

    devShell = forAllSystems (system:
      with nixpkgsFor.${system};
      stdenv.mkDerivation {
        name = "bauer";
        nativeBuildInputs = [ pkgs.emacs git ];
        shellHook = ''
          echo Run ./update.sh to generate files.
        '';
      });

    nixosModules.bauer = {
      imports = [ ./module.nix ];
    };
  };
}
