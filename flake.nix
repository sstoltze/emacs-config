{
  description = "Gathering various flakes and nix/nixos configs";
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixpkgs-unstable"; };
  };
  outputs = { nixpkgs, ... }:
    let
      forEachSystem = (f: nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "x86_64-linux"
      ]
        (system: f { inherit system; pkgs = import nixpkgs { inherit system; }; }));
    in
    {
      nixosModules = {
        default = import ./nix-files/nixos;
        nixosHomeManager = import ./nix-files/home-manager/nixos.nix;
        work = import ./nix-files/home-manager/work.nix;
      };

      # Basic setup for an elixir flake
      elixirDevShells = forEachSystem
        ({ pkgs, ... }:
          let
            credoLanguageServer =
              pkgs.callPackage ./nix-files/credo-language-server.nix { };
          in
          {
            default = pkgs.mkShell
              {
                buildInputs = with pkgs; lib.optional stdenv.isDarwin darwin.apple_sdk.frameworks.CoreServices;
                packages = with pkgs;
                  [ elixir elixir_ls sqlite credoLanguageServer ]
                  ++ lib.optional stdenv.isLinux inotify-tools
                  ++ lib.optional stdenv.isDarwin terminal-notifier
                  ++ lib.optional stdenv.isDarwin fswatch;
              };
          }
        );

      racketSetup = forEachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShell = pkgs.mkShell
            {
              packages = [ pkgs.racket ];
            };
        }
      );
      haskellSetup = forEachSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShell = pkgs.mkShell
            {
              packages = [ pkgs.stack pkgs.ghc ];
            };
        }
      );
    };
}
