{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/flycheck.el;
    extraPackages = epkgs: with epkgs; [
      flycheck
      flycheck-posframe
    ];
  };
}
