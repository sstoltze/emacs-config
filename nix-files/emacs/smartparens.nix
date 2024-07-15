{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/smartparens.el;
    extraPackages = epkgs: [ epkgs.smartparens ];
  };
}
