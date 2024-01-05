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
        credoLanguageServer =
          pkgs.callPackage ./nix-files/credo-language-server.nix { };
      in
      {
        inherit credoLanguageServer;
        devShell = pkgs.mkShell {
          packages = with pkgs;
            [ elixir elixir_ls sqlite credoLanguageServer ]
            ++ pkgs.lib.optional stdenv.isLinux inotify-tools
            ++ pkgs.lib.optional stdenv.isDarwin terminal-notifier;
        };
      });
  };

}
