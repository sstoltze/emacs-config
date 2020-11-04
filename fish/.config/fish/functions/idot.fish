# Defined in - @ line 1
function idot --wraps='dot | icat' --wraps='dot -Ncolor=#aaaaaa -Ecolor=#aaaaaa -Efontcolor=#cccccc -Nfontcolor=#cccccc -Gbgcolor=#181a26 | icat' --description 'alias idot dot -Ncolor=#aaaaaa -Ecolor=#aaaaaa -Efontcolor=#cccccc -Nfontcolor=#cccccc -Gbgcolor=#181a26 | icat'
  dot -Ncolor=#aaaaaa -Ecolor=#aaaaaa -Efontcolor=#cccccc -Nfontcolor=#cccccc -Gbgcolor=#181a26 | icat $argv;
end
