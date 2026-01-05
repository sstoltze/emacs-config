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
    ./tramp.nix
    ./tree-sitter.nix
    ./ui.nix
  ];

  programs.emacs =
    let
      overrides =
        if pkgs.stdenv.isDarwin then
          emacs:
          emacs.override {
            # https://github.com/NixOS/nixpkgs/issues/395169
            withNativeCompilation = false;
          }
        else
          emacs: emacs;
    in
    {
      enable = true;
      package = overrides pkgs.emacs30;
    };
}
