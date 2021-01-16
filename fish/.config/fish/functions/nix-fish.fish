# Defined in - @ line 1
function nix-fish --wraps='nix-shell -p fish --run fish' --description 'alias nix-fish nix-shell -p fish --run fish'
 command nix-shell -p fish --run fish $argv;
end
