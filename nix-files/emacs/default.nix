{ pkgs, ... }:
let
  credoLanguageServer = pkgs.callPackage ../credo-language-server.nix { };
in
{
  imports = [
    (import ./elixir.nix { inherit credoLanguageServer; inherit (pkgs) elixir_ls; })
    ./emacs-options.nix
    ./flycheck.nix
    ./ivy.nix
    ./lsp.nix
    ./magit.nix
    ./nix.nix
    ./projectile.nix
    ./smartparens.nix
    ./ui.nix
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    extraPackages = epkgs: with epkgs; [
      counsel
      counsel-projectile
      diminish
      elixir-ts-mode
      erlang
      flx
      flycheck
      flycheck-posframe
      git-link
      git-timemachine
      heex-ts-mode
      ivy
      ivy-posframe
      ivy-rich
      ivy-xref
      lsp-ivy
      lsp-mode
      magit
      nix-mode
      projectile
      smartparens
      sqlite3
      swiper
      symbol-overlay
      tree-sitter
      treesit-grammars.with-all-grammars
      xref
      pkgs.iosevka
    ];
  };

}
