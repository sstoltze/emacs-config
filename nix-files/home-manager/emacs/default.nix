{ pkgs, ... }:
{
  imports = [
    ./custom-packages.nix
    ./direnv.nix
    ./elixir.nix
    ./emacs-options.nix
    ./flycheck.nix
    ./ivy.nix
    ./latex.nix
    ./lsp.nix
    ./magit.nix
    ./nix.nix
    ./projectile.nix
    ./racket.nix
    ./smartparens.nix
    ./terraform.nix
    ./tramp.nix
    ./tree-sitter.nix
    ./ui.nix
  ];

  programs.emacs = {
    enable = true;
    package =
      if pkgs.stdenv.hostPlatform.isDarwin then
        pkgs.emacs.override {
          # https://github.com/NixOS/nixpkgs/issues/395169
          withNativeCompilation = false;
        }
      else
        pkgs.emacs;
  };
}
