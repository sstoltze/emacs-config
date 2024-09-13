{ pkgs, ... }:

{
  programs.emacs = {
    extraConfig = ''
      (use-package related-files
        :bind (("C-c r" . related-files-find-related-file)))

      (use-package document-sections
        :bind (("C-c d" . document-sections-find-section)))
    '';

    extraPackages = epkgs:
      let
        relatedFiles = pkgs.callPackage ./packages/related-files.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild;
        };
        documentSections = pkgs.callPackage ./packages/document-sections.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild;
        };
      in
      [
        relatedFiles
        documentSections
      ];
  };
}
