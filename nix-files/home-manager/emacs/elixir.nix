{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../../credo-language-server.nix { };
in
{
  programs.emacs = {
    extraConfig = ''
      (use-package tree-sitter
        :defer t
        :custom
        (treesit-language-source-alist
         '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
           (heex "https://github.com/phoenixframework/tree-sitter-heex")
           (gleam "https://github.com/gleam-lang/tree-sitter-gleam")))
        :config
        (dolist (language-source treesit-language-source-alist)
          (unless (treesit-language-available-p (car language-source))
            (treesit-install-language-grammar (car language-source)))))

      (use-package erlang
        :defer t)

      (use-package elixir-ts-mode
        :hook ((elixir-ts-mode . (lambda ()
                                   (add-hook 'before-save-hook 'lsp-format-buffer 0 t))))
        :custom
        (lsp-elixir-server-command '("${pkgs.elixir_ls}/bin/elixir-ls"))
        (lsp-elixir-suggest-specs nil)
        (lsp-credo-version "0.3.0")
        (lsp-credo-command '("${credoLanguageServer}/bin/credo-language-server" "--stdio=true"))
        :config
        (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode)))

      (use-package heex-ts-mode)

      (use-package gleam-ts-mode
        :defer t
        :mode (rx ".gleam" eos)
        :hook ((gleam-ts-mode . (lambda ()
                                  (add-hook 'before-save-hook 'gleam-ts-format nil t)))))
    '';
    extraPackages = epkgs: with epkgs; [
      elixir-ts-mode
      erlang
      gleam-ts-mode
      heex-ts-mode
      tree-sitter
      treesit-grammars.with-all-grammars
    ];
  };
}
