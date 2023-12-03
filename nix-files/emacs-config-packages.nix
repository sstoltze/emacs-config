{ pkgs, ... }: {
  beamPackages = with pkgs.beam_minimal; packagesWith interpreters.erlangR26;
  fontPackages = with pkgs; [ iosevka iosevka-bin ];
  commonPackages = with pkgs; [
    stow
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
    beamPackages.elixir
    (elixir-ls.override { elixir = beamPackages.elixir; })
  ];
  homeManagerPackages = [ pkgs.emacs29 ];
  nixosPackages = with pkgs; [
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
  ];
  systemPackages = with pkgs; [
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
  luaPackages = with pkgs.luaPackages; [ luarocks luadbi-mysql vicious ];
  shellPackage = pkgs.fish;
}
