{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/org.el;
    extraPackages =
      epkgs: with epkgs; [
        org
        org-tree-slide
      ];
  };
}
