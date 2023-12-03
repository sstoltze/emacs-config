{ pkgs, ... }:
let packages = pkgs.callPacakage ./emacs-config-packages.nix { };
in {
  home = {
    # Home Manager needs a bit of information about you and the paths it should
    # manage.
    username = "sarah.stoltze";
    homeDirectory = "/Users/sarah.stoltze";

    # This value determines the Home Manager release that your configuration is
    # compatible with. This helps avoid breakage when a new Home Manager release
    # introduces backwards incompatible changes.
    #
    # You should not change this value, even if you update Home Manager. If you do
    # want to update the value, then make sure to first check the Home Manager
    # release notes.
    stateVersion = "23.05"; # Please read the comment before changing.

    # The home.packages option allows you to install Nix packages into your
    # environment.
    packages = packages.commonPackages ++ packages.fontPackages
      ++ packages.homeManagerPackages;

    # if you don't want to manage your shell through Home Manager.
    sessionVariables = {
      # EDITOR = "emacs";
    };

  };

  fonts.fontconfig.enable = true;

  # Let Home Manager install and manage itself.
  programs = {
    home-manager.enable = true;
    # Maybe?
    # emacs = { enable = true; packages = pkgs.emacs29 }
    # fish.enable = true;
    # kitty.enable = true;
    # ripgrep.enable = true;
  };

  # Disable the "unread news" count when running home-manager switch
  news.display = "silent";
  # Maybe?
  # i18n = {
  #   defaultLocale = "en_GB.UTF-8";

  #   extraLocaleSettings = {
  #     LC_ADDRESS = "da_DK.UTF-8";
  #     LC_IDENTIFICATION = "da_DK.UTF-8";
  #     LC_MEASUREMENT = "da_DK.UTF-8";
  #     LC_MONETARY = "da_DK.UTF-8";
  #     LC_NAME = "da_DK.UTF-8";
  #     LC_NUMERIC = "da_DK.UTF-8";
  #     LC_PAPER = "da_DK.UTF-8";
  #     LC_TELEPHONE = "da_DK.UTF-8";
  #     LC_TIME = "da_DK.UTF-8";
  #   };
  # };
}
