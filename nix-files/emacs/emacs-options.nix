{ pkgs, ... }:

{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/emacs-options.el;
    extraPackages = epkgs: [ pkgs.iosevka ];
  };
}
