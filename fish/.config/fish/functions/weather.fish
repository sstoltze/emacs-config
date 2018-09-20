function weather
    if test -z "$argv"
        set argv "Aarhus"
    end
    curl -s "wttr.in/$argv?M0q"
end
