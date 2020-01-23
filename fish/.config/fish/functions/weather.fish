function weather
    if test -z "$argv"
        set argv "Frederiksberg"
    end
    curl -s "wttr.in/$argv?M0q"
end
