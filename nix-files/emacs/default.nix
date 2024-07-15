{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../credo-language-server.nix { };
in
{
  imports = [
    (import ./elixir.nix { inherit credoLanguageServer; inherit (pkgs) elixir_ls; })
    ./ivy.nix
    ./lsp.nix
    ./magit.nix
    ./nix.nix
    ./projectile.nix
    ./ui.nix
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

}
