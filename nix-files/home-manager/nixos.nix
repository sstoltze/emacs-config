{ pkgs, ... }:
{
  imports = [ ./. ];

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "sst";
    homeDirectory = "/home/sst";
    packages = with pkgs; [
      ardour
      dbeaver-bin
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
      redshift
      skypeforlinux
      slack
      spotify
      sqlite
      steam
      zoom-us
    ];
  };

  programs = {
    home-manager.enable = true;
    git.extraConfig.github.user = "sstoltze";
  };
}
