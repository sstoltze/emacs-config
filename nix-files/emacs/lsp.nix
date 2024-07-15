{ ... }:

{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/lsp.el;
    extraPackages = epkgs: with epkgs; [
      company
      lsp-mode
      lsp-ivy
    ];
  };
}
