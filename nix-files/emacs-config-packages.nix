{ pkgs }:
{
  # Used by both nixos and home-manager
  commonPackages =
    with pkgs;
    [
      direnv
      fish
      git
      graphviz
      jq
      kitty
      kubelogin
      kubie
      nil
      nix-tree
      nixpkgs-fmt
      ripgrep
      stow
      tree-sitter
      (tree-sitter.withPlugins (p: builtins.attrNames p))
    ];
}
