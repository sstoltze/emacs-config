{ beam_minimal
, iosevka
, iosevka-bin
, stow
, jq
, git
, direnv
, kitty
, dbeaver
, ripgrep
, nixpkgs-fmt
, kubie
, kubelogin
, beamPackages
, elixir-ls
, graphviz
, gnome
, redshift
, geoclue2
, blueman
, dropbox
, emacs29-gtk3
, evince
, feh
, firefox
, htop
, networkmanager
, spotify
, sqlite
, discord
, slack
, steam
, zoom-us
, skypeforlinux
, ghc
, stack
, coreutils-full
, racket
, lsof
, emacs29
, gnumake
, xorg
, alsa-firmware
, zip
, unzip
, sof-firmware
, linuxPackagesFor
, pulseaudioFull
, fish
, linux_latest
, luaPackages
, callPackage
}: {
  # Fonts
  fontPackages = [ iosevka iosevka-bin ];
  # Used by both nixos and home-manager
  commonPackages =
    let
      beamPackages = with beam_minimal; packagesWith interpreters.erlangR26;
      credoLanguageServer = callPackage ./credo-language-server.nix { };
    in
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
      beamPackages.elixir
      (elixir-ls.override { elixir = beamPackages.elixir; })
      credoLanguageServer
      graphviz
      fish
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
    ghc
    stack
    racket
    lsof
  ];

  # Various nixos required setup
  nixosConfig = {
    systemPackages = [
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
