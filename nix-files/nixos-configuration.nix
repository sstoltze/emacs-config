# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ emacsConfigPackages }:
{ config, pkgs, ... }:
let
  packages = emacsConfigPackages pkgs;
  # # This, with the nixpkgs.config below, provies pkgs.unstable.<pkg-name>
  # # for a single unstable package installation
  # let
  #   unstableTarball = fetchTarball
  #     "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  # in
in {

  # nixpkgs.config = {
  #   packageOverrides = pkgs: {
  #     unstable = import unstableTarball { config = config.nixpkgs.config; };
  #   };
  # };

  # Bootloader
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # Enable sound?
    kernelPackages = packages.kernelPackages;
  };

  networking = {
    hostName = "nixos"; # Define your hostname.

    # Enable networking
    networkmanager.enable = true;

    # Configure network proxy if necessary
    # proxy.default = "http://user:password@proxy:port/";
    # proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Open ports in the firewall.
    # firewall.allowedTCPPorts = [ ... ];
    # firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # firewall.enable = false;
  };

  hardware = {
    enableAllFirmware = true;

    # Bluetooth
    bluetooth = {
      enable = true;
      powerOnBoot = true; # powers up the default Bluetooth controller on boot
    };

    pulseaudio = {
      enable = true;
      package = packages.pulseaudioPackages;
      # Automatically switch to bluetooth speaker on connect
      extraConfig = "load-module module-switch-on-connect";
    };
  };

  sound.enable = true;

  nixpkgs.config = {
    # Allow unfree packages
    allowUnfree = true;
    pulseaudio = true;
  };

  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
    optimise.automatic = true;
  };

  services = {

    xserver = {
      enable = true;

      # Configure keymap in X11
      layout = "dk";
      xkbVariant = "nodeadkeys";

      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+awesome";
      };

      windowManager.awesome = {
        enable = true;
        luaModules = packages.luaPackages;
      };
    };

    blueman.enable = true;

    # Fingerprint reader
    # fprintd = {
    #   enable = true;
    #   tod = {
    #     enable = true;
    #     driver = pkgs.libfprint-2-tod1-goodix-550a;
    #   };
    # };

    # emacs.defaultEditor = true;
    # Enable the OpenSSH daemon.
    # openssh.enable = true;
  };

  # Configure console keymap
  console.keyMap = "dk-latin1";
  fonts = { packages = packages.fontPackages; };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sst = {
    isNormalUser = true;
    description = "Sarah Ella Stoltze";
    extraGroups = [ "networkmanager" "wheel" "input" "audio" ];
    shell = packages.shellPackages;
    packages = packages.commonPackages ++ packages.nixosPackages;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # These packages are available to users system-wide
  environment.systemPackages = packages.systemPackages;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs = {
    fish = { enable = true; };
    steam = {
      enable = true;
      # Open ports in the firewall for Steam Remote Play
      remotePlay.openFirewall = true;
      # Open ports in the firewall for Source Dedicated Server
      dedicatedServer.openFirewall = true;
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Copenhagen";

  # Select internationalisation properties.
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
