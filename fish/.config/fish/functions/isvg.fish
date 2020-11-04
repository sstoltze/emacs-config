# Defined in - @ line 1
function isvg --wraps='rsvg-convert | icat' --description 'alias isvg rsvg-convert | icat'
  rsvg-convert | icat $argv;
end
