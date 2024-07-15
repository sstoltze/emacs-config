{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../credo-language-server.nix { };
in
{
  imports = [ ./lsp.nix ./projectile.nix (import ./elixir.nix { inherit credoLanguageServer; }) ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };

}
