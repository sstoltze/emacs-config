{ alsa-firmware
, blueman
, callPackage
, coreutils-full
, dbeaver
, direnv
, discord
, dropbox
, emacs29
, emacs29-gtk3
, evince
, feh
, firefox
, fish
, geoclue2
, git
, gnome
, gnumake
, graphviz
, htop
, iosevka
, iosevka-bin
, jq
, kitty
, kubelogin
, kubie
, lib
, linuxPackagesFor
, linux_latest
, lsof
, luaPackages
, networkmanager
, nix-tree
, nixpkgs-fmt
, pulseaudioFull
, redshift
, ripgrep
, skypeforlinux
, slack
, sof-firmware
, spotify
, sqlite
, steam
, stow
, tree-sitter
, unzip
, xorg
, zip
, zoom-us
}:
{
  # Fonts
  fontPackages = [ iosevka iosevka-bin ];
  # Used by both nixos and home-manager
  commonPackages =
    [
      stow
      jq
      git
      direnv
      kitty
      dbeaver
      ripgrep
      nixpkgs-fmt
      kubie
      kubelogin
      graphviz
      fish
      nix-tree
      tree-sitter
      (tree-sitter.withPlugins (p: builtins.attrNames p))
    ];

  # Unique to home-manager
  homeManagerPackages = [ emacs29 ];
  # Unique to nixos
  nixosPackages = [
    gnome.gnome-keyring
    redshift
    geoclue2
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
    discord
    slack
    steam
    zoom-us
    skypeforlinux
    lsof
  ];

  # Various nixos required setup
  nixosConfig = {
    systemPackages = [
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
    pulseaudioPackage = pulseaudioFull;
    kernelPackages = linuxPackagesFor linux_latest;
    luaPackages = with luaPackages; [ luarocks luadbi-mysql vicious ];
    shellPackage = fish;
  };

}
