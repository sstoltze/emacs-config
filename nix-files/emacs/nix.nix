{ pkgs, ... }:

{
  programs.emacs = {
    extraConfig = ''
      (use-package nix-mode
        :ensure t
        :defer t
        :hook ((nix-mode . (lambda ()
                             (add-hook 'before-save-hook 'nix-format-before-save 0 t))))
        :custom
        (nix-nixfmt-bin "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"))
    '';
    extraPackages = epkgs: [ epkgs.nix-mode ];
  };
}
