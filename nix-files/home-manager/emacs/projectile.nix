{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/projectile.el;
    extraPackages = epkgs: with epkgs; [
      projectile
      counsel-projectile
    ];
  };
}
