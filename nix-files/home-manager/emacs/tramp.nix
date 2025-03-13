{ ... }:
{
  programs.emacs.extraConfig = builtins.readFile ./elisp/tramp.el;
}
