{ pkgs, ... }:
{
  imports = [ ./. ];

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "sst";
    homeDirectory = "/home/sst";
    packages = with pkgs; [
      dbeaver-bin
      deluge
      discord
      dropbox
      evince
      feh
      firefox
      geoclue2
      gnome-keyring
      htop
      lsof
      networkmanager
      proton-authenticator
      proton-pass
      protonvpn-gui
      redshift
      slack
      spotify
      sqlite
      steam
      vlc
      zoom-us
    ];
  };

  programs = {
    home-manager.enable = true;
    git.settings.github.user = "sstoltze";
  };
}
