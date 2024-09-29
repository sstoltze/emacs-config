{ pkgs, ... }:
let
  packages = pkgs.callPackage ../emacs-config-packages.nix { };
in
{
  imports = [
    ./emacs
    ./git.nix
    ./kitty.nix
    ./fish.nix
  ];

  home = {
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
    fish.enable = true;
    ripgrep.enable = true;
  };

  # Disable the "unread news" count when running home-manager switch
  news.display = "silent";
}
