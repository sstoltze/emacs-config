{
  inputs = {
    emacsConfig = {
      url = "git+https://github.com/sstoltze/emacs-config?dir=nixos/etc/nixos/";
    };
  };

  outputs = { self, nixpkgs, emacsConfig }: {
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ emacsConfig.nixosConfig ];
    };
  };
}
