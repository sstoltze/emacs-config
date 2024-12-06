{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/ui.el;
    extraPackages =
      epkgs: with epkgs; [
        diminish
        symbol-overlay
        visible-mark
      ];
  };
}
