{ ... }:
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

      (use-package yaml-ts-mode)

      (use-package expand-region
        :bind ("C-c SPC" . er/expand-region))
    '';
    extraPackages =
      epkgs: with epkgs; [
        tree-sitter
        treesit-grammars.with-all-grammars
        expand-region
      ];
  };
}
