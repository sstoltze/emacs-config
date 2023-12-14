{
  description = "Gathering various flakes and nix/nixos configs";
  inputs = { flake-utils = { url = "github:numtide/flake-utils"; }; };
  outputs = { self, flake-utils, nixpkgs, ... }: {
    nixosConfig = import ./nix-files/nixos-configuration.nix;

    homeManagerConfig = import ./nix-files/home-manager-configuration.nix;

    # Basic setup for an elixir flake
    elixirSetup = flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        systemPackages = if (builtins.any (s: s == system) [
          "x86_64-darwin"
          "aarch64-darwin"
        ]) then
          [ ]
        else
          [ pkgs.inotify-tools ];
      in {
        devShell = pkgs.mkShell {
          packages = with pkgs; [ elixir elixir_ls sqlite ] ++ systemPackages;
        };
      });
  };

}
