{
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    emacsConfig = {
      url = "/home/sst/git/emacs-config";
    };
    homeManager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "emacsConfig/nixpkgs";
    };
  };

  outputs = { self, nixpkgs, emacsConfig, homeManager }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        emacsConfig.nixosModules.default
        ./hardware-configuration.nix
        homeManager.nixosModules.home-manager
        {
          home-manager.backupFileExtension = "mine";
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.sst = emacsConfig.nixosModules.nixosHomeManager;
        }
      ];
    };
  };
}
