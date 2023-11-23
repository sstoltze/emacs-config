{
  description = "Gathering various flakes and nix/nixos configs";

  outputs = { self, ... }: {
    nixosConfig = import ./nix-files/nixos-configuration.nix { };

    homeManagerConfig = import ./nix-files/home-manager-configuration.nix { };
  };

}
