{ pkgs, ... }:
{
  programs.emacs = {
    extraPackages = epkgs: [
      (pkgs.callPackage
        ./packages/related-files.nix
        { inherit (pkgs) fetchFromGitHub; inherit (epkgs) trivialBuild; })
      (pkgs.callPackage
        ./packages/document-sections.nix
        { inherit (pkgs) fetchFromGitHub; inherit (epkgs) trivialBuild; })
    ];
  };
}
