{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.lib.mkForce pkgs.iosevka;
      name = pkgs.lib.mkForce "Iosevka Term";
      size = 14;
    };
  };
}
