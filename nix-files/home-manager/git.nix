{ pkgs, ... }:
{
  programs.git = {
    enable = true;
    extraConfig = {
      core = {
        autocrlf = "input";
        editor = "emacs";
        pager = "${pkgs.delta}/bin/delta";
      };
      credential.helper = "store";
      diff = {
        algorithm = "histogram";
        colorMoved = "default";
        defaultBranch = "main";
      };
      merge.conflictStyle = "zdiff3";
      pull.rebase = "true";
      push.autoSetupRemote = "true";
      interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
      delta = {
        hyperlinks = true;
        navigate = true;
        side-by-side = true;
        syntax-theme = "zenburn";
      };
    };
  };
}
