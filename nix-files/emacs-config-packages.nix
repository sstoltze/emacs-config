{ alsa-firmware
, beamPackages
, beam_minimal
, blueman
, callPackage
, coreutils-full
, dbeaver
, direnv
, discord
, dropbox
, elixir-ls
, emacs29
, emacs29-gtk3
, evince
, feh
, firefox
, fish
, geoclue2
, ghc
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
, linuxPackagesFor
, linux_latest
, lsof
, luaPackages
, networkmanager
, nix-tree
, nixpkgs-fmt
, pulseaudioFull
, racket
, redshift
, ripgrep
, skypeforlinux
, slack
, sof-firmware
, spotify
, sqlite
, stack
, steam
, stow
, unzip
, xorg
, zip
, zoom-us
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
      nix-tree
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
