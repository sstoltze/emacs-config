function fish_prompt --description 'Write out the prompt'
    set -l prompt_color $fish_color_normal
    if test $status -ne 0
        set prompt_color "red"
    end

    set -l time (printf "%s%s%s" (set_color $prompt_color) (date +%R) (set_color normal))

    set -l user (printf "%s$USER%s" (set_color -u $fish_color_user) (set_color normal))

    # Working directory
    set -l home_escaped (echo -n $HOME | sed 's/\//\\\\\//g')
    set -l wd (echo -n $PWD | sed "s/^$home_escaped/~/" | sed 's/ /%20/g')

    # Shorten when too long
    if test (math (string length $pwd+$prompt_symbol+(date +%R)+$USER) + 15) -gt $COLUMNS
        set wd (prompt_pwd)
    end

    # Include nix shell when appropriate
    if test -n "$IN_NIX_SHELL"
        set wd (printf "%s[nix-shell:$wd]%s" (set_color $fish_color_nix) (set_color normal))
    end

    set wd (printf "%s$wd%s%s" (set_color $fish_color_cwd) (set_color normal) (__fish_vcs_prompt))

    # Include k8s context when enabled by 'kubeon'
    if test -n "$K8S_FISH_PROMPT"
        set wd (printf "%s[k8s:%s]%s $wd" (set_color $fish_color_kubernetes) (kubectl config current-context) (set_color normal))
    end

    # The space before the '>' is needed due to aggresive ligatures
    set -l prompt_symbol (printf "%s>%s " (set_color $prompt_color) (set_color normal))

    printf "%s %s %s %s" $time $user $wd $prompt_symbol
end
