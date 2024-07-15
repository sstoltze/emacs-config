{ ... }:
{
  programs.emacs.extraConfig = ''
    (load-theme 'deeper-blue t)
    (set-face-background 'cursor "burlywood")

    (use-package symbol-overlay
      :ensure t
      :defer t
      :diminish symbol-overlay-mode
      :hook ((prog-mode . symbol-overlay-mode)))
  '';
}
