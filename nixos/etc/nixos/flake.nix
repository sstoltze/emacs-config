{
  inputs = {
    # nixpkgs = { url = "github:NixOS/nixpkgs"; };
    emacsConfig = {
      url = "/home/sst/git/emacs-config";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, emacsConfig }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ emacsConfig.nixosConfig ./hardware-configuration.nix ];
    };
  };
}
