{ ... }:

{
  programs.emacs = {
    extraConfig = builtins.readFile ./elisp/magit.el;
    extraPackages = epkgs: with epkgs; [
      git-link
      git-timemachine
      magit
      sqlite3
    ];
  };

}
