{ credoLanguageServer, elixir_ls, ... }:
{ ... }:

{
  programs.emacs.extraConfig = ''
    (use-package tree-sitter
      :defer t
      :custom
      (treesit-language-source-alist
       '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
         (heex "https://github.com/phoenixframework/tree-sitter-heex")))
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
      (lsp-elixir-server-command '("${elixir_ls}/bin/elixir-ls"))
      (lsp-elixir-suggest-specs nil)
      (lsp-credo-version "0.3.0")
      (lsp-credo-command '("${credoLanguageServer}/bin/credo-language-server" "--stdio=true"))
      :config
      (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode)))

     (use-package heex-ts-mode)
  '';
}
