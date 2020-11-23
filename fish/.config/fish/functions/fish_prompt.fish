function fish_prompt --description 'Write out the prompt'
    set -l prompt_color $fish_color_normal
    if test $status -ne 0
        set prompt_color "red"
    end

    set -l home_escaped (echo -n $HOME | sed 's/\//\\\\\//g')
    set -l wd (echo -n $PWD | sed "s/^$home_escaped/~/" | sed 's/ /%20/g')
    set -l prompt_symbol ' >'

    if test (math (string length $pwd+$prompt_symbol+(date +%R)+$USER) + 15) -gt $COLUMNS
        set wd (prompt_pwd)
    end

    if test -n "$IN_NIX_SHELL"
        set wd (printf "%s[nix-shell:$wd]%s" (set_color $fish_color_nix) (set_color normal))
    end

    printf "%s%s %s%s%s %s%s%s%s%s%s%s " (set_color $prompt_color) (date +%R) (set_color -u $fish_color_user) $USER (set_color normal) (set_color $fish_color_cwd) $wd (set_color normal) (__fish_vcs_prompt) (set_color $prompt_color) $prompt_symbol (set_color normal)
end
