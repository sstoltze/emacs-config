{ pkgs, ... }:

{
  home.packages = [ pkgs.nodejs ];
  programs.emacs = {
    extraConfig = ''
      ${builtins.readFile ./elisp/lsp.el}

      (use-package lsp-tailwindcss
        :after lsp-mode
        :custom
        (lsp-tailwindcss-server-path "${pkgs.lib.getExe pkgs.tailwindcss-language-server}")
        :config
        (add-to-list 'lsp-tailwindcss-major-modes 'elixir-ts-mode))
    '';
    extraPackages =
      epkgs: with epkgs; [
        company
        lsp-mode
        lsp-ivy
        lsp-tailwindcss
      ];
  };
}
