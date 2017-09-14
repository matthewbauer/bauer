
{
  packageOverrides = pkgs: with pkgs; rec {
    bauer = import ./default.nix { nixpkgs_ = pkgs; };
    userPackages = bauer;
  };
}
