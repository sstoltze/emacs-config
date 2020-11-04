# Defined in - @ line 1
function icat --wraps='kitty icat --align=left' --description 'alias icat kitty icat --align=left'
  kitty icat --align=left $argv;
end
