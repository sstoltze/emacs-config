{ config, pkgs, ... }:

let beamPackages =
        with pkgs.beam_minimal;
        packagesWith interpreters.erlangR26;
    elixir = beamPackages.elixir_1_15;
in

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "sarah.stoltze";
  home.homeDirectory = "/Users/sarah.stoltze";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    pkgs.fish
    pkgs.iosevka
    pkgs.iosevka-bin
    pkgs.emacs29
    pkgs.jq
    pkgs.git
    pkgs.direnv
    pkgs.kitty

    # Kubie
    pkgs.kubie
    pkgs.kubelogin

    # Elixir
    elixir
    (pkgs.elixir-ls.override { elixir = elixir; })
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    ".emacs.d/init.el".source = ../../../emacs/.emacs.d/init.el;
    ".gitconfig".source = ../../../git/.gitconfig;
    ".config/git".source = ../../../git/.config/git;
    ".config/fish".source = ../../../fish/.config/fish;
    ".config/kitty".source = ../../../kitty/.config/kitty;
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/sarah.stoltze/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
