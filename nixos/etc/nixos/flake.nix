{
  inputs = {
    emacsConfig = { url = "git+https://github.com/sstoltze/emacs-config"; };
  };

  outputs = { self, nixpkgs, emacsConfig }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ emacsConfig.nixosConfig ./hardware-configuration.nix ];
    };
  };
}
