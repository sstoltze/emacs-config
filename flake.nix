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
        beamPackages = with pkgs.beam_minimal; packagesWith interpreters.erlangR26;
        credoLanguageServer =
          pkgs.callPackage ./nix-files/credo-language-server.nix { };
        elixir = beamPackages.elixir_1_16;
      in
      {
        inherit credoLanguageServer;
        beamPackages = beamPackages // {
          buildMix = beamPackages.buildMix.override {
            inherit elixir;
            erlang = beamPackages.erlang;
            hex = beamPackages.hex.override { inherit elixir; };
          };
        };
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; lib.optional stdenv.isDarwin darwin.apple_sdk.frameworks.CoreServices;
          packages = with pkgs;
            [ elixir elixir_ls sqlite credoLanguageServer ]
            ++ lib.optional stdenv.isLinux inotify-tools
            ++ lib.optional stdenv.isDarwin terminal-notifier
            ++ lib.optional stdenv.isDarwin fswatch;
        };
      });
    racketSetup = flake-utils.lib.eachDefaultSystem (system:
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
    haskellSetup = flake-utils.lib.eachDefaultSystem (system:
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
