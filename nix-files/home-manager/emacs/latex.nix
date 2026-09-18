{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/latex.el;
    extraPackages =
      epkgs: with epkgs; [
        auctex
      ];
  };

}
