{ pkgs, ... }:
{
  programs.emacs =
    if pkgs.stdenv.isDarwin then
      { }
    else
      {
        extraConfig = builtins.readFile ./elisp/org.el;
        extraPackages =
          epkgs: with epkgs; [
            org
            org-tree-slide
          ];
      };
}
