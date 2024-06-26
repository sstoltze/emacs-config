# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
let
  packages = pkgs.callPackage ./emacs-config-packages.nix { };
  # # This, with the nixpkgs.config below, provies pkgs.unstable.<pkg-name>
  # # for a single unstable package installation
  # let
  #   unstableTarball = fetchTarball
  #     "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
  # in
in
{

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
    kernelPackages = packages.nixosConfig.kernelPackages;
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
      package = packages.nixosConfig.pulseaudioPackage;
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

    displayManager = {
      defaultSession = "none+awesome";
    };
    xserver = {
      enable = true;

      # Configure keymap in X11
      xkb = {
        variant = "nodeadkeys";
        layout = "dk";
      };

      displayManager = {
        lightdm.enable = true;

        sessionCommands = ''
          ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
            Xcursor.theme: Adwaita
            Xcursor.size: 32
          EOF
        '';
      };

      windowManager.awesome = {
        enable = true;
        luaModules = packages.nixosConfig.luaPackages;
      };
    };

    redshift = {
      enable = true;
      # A basic copy of @related [redshift.conf](redshift/.config/redshift/redshift.conf)
      brightness = {
        day = "0.8";
        night = "0.8";
      };
      temperature = {
        day = 4200;
        night = 3800;
      };

      extraOptions = [
        "-r" # No fading
      ];
    };
    geoclue2 = { enable = true; };

    blueman.enable = true;

    # Needed for skype, otherwise complains about missing secret
    gnome.gnome-keyring = { enable = true; };

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
  location.provider = "geoclue2";

  # Configure console keymap
  console.keyMap = "dk-latin1";
  fonts = { packages = packages.fontPackages; };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sst = {
    isNormalUser = true;
    description = "Sarah Ella Stoltze";
    extraGroups = [ "networkmanager" "wheel" "input" "audio" ];
    shell = packages.nixosConfig.shellPackage;
    packages = packages.commonPackages ++ packages.nixosPackages;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # These packages are available to users system-wide
  environment.systemPackages = packages.nixosConfig.systemPackages;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs = {
    dconf = { enable = true; };
    fish = { enable = true; };
    steam = {
      enable = true;
      # Open ports in the firewall for Steam Remote Play
      remotePlay.openFirewall = true;
      # Open ports in the firewall for Source Dedicated Server
      dedicatedServer.openFirewall = true;
    };
    git = {
      enable = true;
      config = {
        core.pager = "${pkgs.delta}/bin/delta";
        diff.colorMoved = "default";
        interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
        delta = {
          hyperlinks = true;
          navigate = true;
          side-by-side = true;
          syntax-theme = "zenburn";
        };
      };
    };
  };

  fileSystems = {
    "/media/kobo" = {
      device = "/dev/disk/by-uuid/5F45-F1AB";
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
