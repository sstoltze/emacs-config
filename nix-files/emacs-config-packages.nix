{ pkgs }:
{
  # Fonts
  fontPackages = with pkgs; [ iosevka iosevka-bin ];
  # Used by both nixos and home-manager
  commonPackages =
    with pkgs; [
      direnv
      fish
      git
      graphviz
      jq
      kitty
      kubelogin
      kubie
      nil
      nix-tree
      nixpkgs-fmt
      ripgrep
      stow
      tree-sitter
      (tree-sitter.withPlugins (p: builtins.attrNames p))
    ];

  # Unique to home-manager
  homeManagerPackages = with pkgs; [ delta ];
  # Unique to nixos
  nixosPackages = with pkgs; [
    blueman
    dbeaver-bin
    discord
    dropbox
    emacs29-gtk3
    evince
    feh
    firefox
    geoclue2
    gnome.gnome-keyring
    htop
    lsof
    networkmanager
    redshift
    skypeforlinux
    slack
    spotify
    sqlite
    steam
    zoom-us
  ];

  # Various nixos required setup
  nixosConfig = {
    systemPackages = with pkgs; [
      awesome
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
    ];
    pulseaudioPackage = pkgs.pulseaudioFull;
    kernelPackages = pkgs.linuxPackagesFor pkgs.linux_latest;
    luaPackages = with pkgs.luaPackages; [ vicious ];
    shellPackage = pkgs.fish;
  };

}
