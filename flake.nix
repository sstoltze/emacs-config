{
  description = "Gathering various flakes and nix/nixos configs";
  inputs = { flake-utils = { url = "github:numtide/flake-utils"; }; };
  outputs = { self, flake-utils, nixpkgs, ... }:

    let
      beamPackages =
        (pkgs: with pkgs.beam_minimal; packagesWith interpreters.erlangR26);
      fontPackages = (pkgs: with pkgs; [ iosevka iosevka-bin ]);
      commonPackages = (pkgs:
        with pkgs; [
          stow
          fish
          jq
          git
          direnv
          kitty
          dbeaver
          ripgrep
          nixfmt

          # Kubie
          kubie
          kubelogin

          # Elixir
          (beamPackages pkgs).elixir
          (elixir-ls.override { elixir = (beamPackages pkgs).elixir; })
        ]);
      homeManagerPackages = (pkgs: [ pkgs.emacs29 ]);
      nixosPackages = (pkgs:
        with pkgs; [
          blueman
          dropbox
          emacs29-gtk3
          evince
          feh
          firefox
          htop
          networkmanager
          spotify
          sqlite
          # social
          discord
          slack
          steam
          zoom-us
          skypeforlinux
          # haskell
          ghc
          stack
          # racket
          racket
          lsof
        ]);
      systemPackages = (pkgs:
        with pkgs; [
          git
          coreutils-full
          gnumake
          # fprintd # Fingerprint reader
          xorg.xmodmap
          alsa-firmware
          pulseaudioFull
          zip
          unzip
          sof-firmware
        ]);
      kernelPackages = (pkgs: pkgs.linuxPackagesFor pkgs.linux_latest);
      luaPackages =
        (pkgs: with pkgs.luaPackages; [ luarocks luadbi-mysql vicious ]);
    in {
      nixosConfig = import ./nix-files/nixos-configuration.nix {
        packages = (pkgs: commonPackages pkgs ++ nixosPackages pkgs);
        shell = (pkgs: pkgs.fish);
        pulseAudio = (pkgs: pkgs.pulseaudioFull);
        inherit fontPackages systemPackages kernelPackages luaPackages;
      };

      homeManagerConfig = import ./nix-files/home-manager-configuration.nix {
        packages = (pkgs:
          commonPackages pkgs ++ fontPackages pkgs ++ homeManagerPackages pkgs);
      };
    };

}
