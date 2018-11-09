#!/usr/bin/fish
# 'Prints a list of colours usable in fish'
function color-test --description 'Prints a list of colours usable in fish'
    for color in (set_color -c | rev | sort | rev)
        set_color $color
        echo -n "$color"
        set_color normal
        for option in '-d' '-o' '-i' '-u' '-r' '-b'
            echo -n "   "
            eval "set_color $option $color"
            echo -n "$color$option"
            set_color normal
        end
        echo
    end
end
