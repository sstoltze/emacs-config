{ ... }:
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/ivy.el;
    extraPackages = epkgs: with epkgs; [
      counsel
      flx
      ivy
      ivy-posframe
      ivy-rich
      ivy-xref
      lsp-ivy
      swiper
      xref
    ];
  };


}
