{
  programs.emacs = {
    extraConfig = ''
      (use-package direnv
        :config
        (direnv-mode))
    '';
    extraPackages = epkgs: [ epkgs.direnv ];
  };
}
