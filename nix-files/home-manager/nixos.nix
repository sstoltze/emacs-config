{ ... }:
{
  imports = [ ./. ];

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "sst";
    homeDirectory = "/home/sst";
  };

  programs.home-manager.enable = true;
}
