{
  packageOverrides = pkgs: with pkgs; rec {
    bauer = import ./default.nix { nixpkgs' = pkgs; };
    userPackages = bauer;
  };
}
