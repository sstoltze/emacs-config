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
        credoLanguageServer = pkgs.stdenv.mkDerivation {
          name = "credo-language-server";

          src = pkgs.fetchFromGitHub {
            owner = "elixir-tools";
            repo = "credo-language-server";
            # Release 0.3.0
            rev = "106be2476073d5ec85ed1695dc2b11e94abf650a";
            hash = "sha256-YrwLzQTwtMNiaIT0Ar59PwsT0q+lhAVNvBKAlewHZ2Y=";
          };

          installPhase = ''
            mkdir -p "$out"/bin
            cp -a "$src"/bin/credo-language-server "$out"/bin/credo-language-server
            chmod +x "$out"/bin/credo-language-server
          '';

          dontFixup = true;
        };
      in {
        credoLanguageServer = credoLanguageServer;
        devShell = pkgs.mkShell {
          packages = with pkgs;
            [ elixir elixir_ls sqlite credoLanguageServer ]
            ++ pkgs.lib.optional stdenv.isLinux inotify-tools
            ++ pkgs.lib.optional stdenv.isDarwin terminal-notifier;
        };
      });
  };

}
