{ pkgs, ... }:
{
  imports = [
    ./custom-packages.nix
    ./direnv.nix
    ./elixir.nix
    ./emacs-options.nix
    ./flycheck.nix
    ./ivy.nix
    ./lsp.nix
    ./magit.nix
    ./nix.nix
    ./org.nix
    ./projectile.nix
    ./smartparens.nix
    ./terraform.nix
    ./tramp.nix
    ./tree-sitter.nix
    ./ui.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30;
  };

}
