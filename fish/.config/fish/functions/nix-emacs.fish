# Defined in - @ line 1
function nix-emacs --wraps='nix-shell -p emacs27 --run "emacs"' --description 'alias nix-emacs=nix-shell -p emacs27 --run "emacs"'
  nix-shell -p emacs27 --run "emacs" $argv;
end
