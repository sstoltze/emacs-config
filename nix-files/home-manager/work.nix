{ ... }:
{
  modules = [ ./defaults.nix ];

  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "sarah.stoltze";
    homeDirectory = "/Users/sarah.stoltze";
  };
}
