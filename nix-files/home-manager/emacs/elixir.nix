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

      (defvar-keymap sstoltze-mix-test
        "t" #'(lambda () (interactive) (sstoltze/lsp-run-mix-test :point))
        "r" #'(lambda () (interactive) (sstoltze/lsp-run-mix-test :last))
        "s" #'(lambda () (interactive) (sstoltze/lsp-run-mix-test :suite))
        "f" #'(lambda () (interactive) (sstoltze/lsp-run-mix-test :file)))

      (use-package elixir-ts-mode
        :hook ((elixir-ts-mode . (lambda ()
                                   (add-hook 'before-save-hook 'lsp-format-buffer 0 t))))
        :bind-keymap ("C-c t" . sstoltze-mix-test)
        :custom
        (lsp-elixir-server-command '("elixir-ls"))
        (lsp-elixir-suggest-specs nil)
        (lsp-elixir-fetch-deps t)
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
      (use-package yaml-ts-mode)
    '';

    extraPackages =
      epkgs: with epkgs; [
        elixir-ts-mode
        erlang
        gleam-ts-mode
        heex-ts-mode
        tree-sitter
        treesit-grammars.with-all-grammars
      ];
  };
}
