{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../../credo-language-server.nix { };
in
{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/elixir.el;
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
