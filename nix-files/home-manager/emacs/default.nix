{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../../credo-language-server.nix { };
in
{
  imports = [
    ./custom-packages.nix
    ./direnv.nix
    (import ./elixir.nix { inherit credoLanguageServer; inherit (pkgs) elixir_ls; })
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
    package = pkgs.lib.mkForce pkgs.emacs29;
  };

}
