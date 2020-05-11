# Defined in - @ line 1
function nix-shell --wraps='nix-shell -p fish --run fish' --description 'alias nix-shell nix-shell -p fish --run fish'
 command nix-shell -p fish --run fish $argv;
end
