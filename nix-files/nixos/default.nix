{ pkgs, ... }:
let
  packages = pkgs.callPackage ../emacs-config-packages.nix { };
in
{
  imports = [
    ./bluetooth.nix
    ./display.nix
    ./ereader.nix
    ./nix.nix
    ./sound.nix
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
  };

  networking = {
    hostName = "nixos";

    networkmanager.enable = true;
  };

  hardware = {
    enableAllFirmware = true;
  };

  services = {
    xserver = {
      enable = true;

      xkb = {
        variant = "nodeadkeys";
        layout = "dk";
      };
    };

    geoclue2 = { enable = true; };

    # Needed for skype, otherwise complains about missing secret
    gnome.gnome-keyring = { enable = true; };
  };

  location.provider = "geoclue2";

  console.keyMap = "dk-latin1";
  fonts.packages = with pkgs; [ iosevka iosevka-bin ];

  users.users.sst = {
    isNormalUser = true;
    description = "Sarah Ella Stoltze";
    extraGroups = [ "networkmanager" "wheel" "input" "audio" ];
    shell = pkgs.fish;
    packages = packages.commonPackages;
  };

  environment.systemPackages = with pkgs; [
    stow
    git
    coreutils-full
    gnumake
    # fprintd # Fingerprint reader
    xorg.xmodmap
    alsa-firmware
    zip
    unzip
    sof-firmware
    usbutils
    qpwgraph
  ];

  programs = {
    dconf = { enable = true; };
    fish = { enable = true; };
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };
  };

  time.timeZone = "Europe/Copenhagen";

  i18n = {
    defaultLocale = "en_GB.UTF-8";

    extraLocaleSettings = {
      LC_ADDRESS = "da_DK.UTF-8";
      LC_IDENTIFICATION = "da_DK.UTF-8";
      LC_MEASUREMENT = "da_DK.UTF-8";
      LC_MONETARY = "da_DK.UTF-8";
      LC_NAME = "da_DK.UTF-8";
      LC_NUMERIC = "da_DK.UTF-8";
      LC_PAPER = "da_DK.UTF-8";
      LC_TELEPHONE = "da_DK.UTF-8";
      LC_TIME = "da_DK.UTF-8";
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
