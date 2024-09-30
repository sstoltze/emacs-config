{ ... }:
{
  programs.emacs = {
    extraConfig = ''
      (use-package terraform-mode
        :defer t
        :hook ((terraform-mode . terraform-format-on-save-mode)))
    '';

    extraPackages = epkgs: with epkgs; [
      terraform-mode
    ];
  };
}
