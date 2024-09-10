{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.lib.mkForce pkgs.iosevka;
      name = pkgs.lib.mkForce "Iosevka Term";
      size = 14;
    };
    keybindings = {
      "ctrl+shift+o" = "layout_action rotate";
      "ctrl+shift+up" = "move_window up";
      "ctrl+shift+down" = "move_window down";
      "ctrl+shift+left" = "move_window left";
      "ctrl+shift+right" = "move_window right";
    };
    settings = {
      # aci-modified.conf
      color0 = "#363636";
      color1 = "#cd5c5c";
      color2 = "#138034";
      color3 = "#eedd82";
      color4 = "#0883ff";
      color5 = "#cd00cd";
      color6 = "#98f5ff";
      color7 = "#cccccc";
      color8 = "#424242";
      color9 = "#ff0000";
      color10 = "#9acd32";
      color11 = "#ffed1e";
      color12 = "#1e8eff";
      color13 = "#8e1eff";
      color14 = "#7ac5cd";
      color15 = "#c2c2c2";
      background = "#181a26";
      foreground = "#cccccc";
      cursor = "#eedd82";
    };
  };
}
