{
  description = "Gathering various flakes and nix/nixos configs";
  inputs = { flake-utils = { url = "github:numtide/flake-utils"; }; };
  outputs = { self, flake-utils, nixpkgs, ... }:

    let emacsConfigPackages = import ./nix-files/emacs-config-packages.nix;
    in {
      nixosConfig = import ./nix-files/nixos-configuration.nix {
        inherit emacsConfigPackages;
      };

      homeManagerConfig = import ./nix-files/home-manager-configuration.nix {
        inherit emacsConfigPackages;
      };
    };

}
