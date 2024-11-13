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
    ./projectile.nix
    ./smartparens.nix
    ./terraform.nix
    ./ui.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

}
