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

    # Include screen info when appropriate
    if test (string sub -l 6 "$TERM") = 'screen'
        set -l host (string split '.' "$STY")[3]
        set wd (printf "%s[screen@$host:$wd]%s" (set_color $fish_color_nix) (set_color normal))
    end

    # Include nix shell when appropriate
    if test -n "$IN_NIX_SHELL"
        set wd (printf "%s[nix-shell:$wd]%s" (set_color $fish_color_nix) (set_color normal))
    end

    # Include guix shell when appropriate
    if test -n "$GUIX_ENVIRONMENT"
        set wd (printf "%s[guix:$wd]%s" (set_color $fish_color_nix) (set_color normal))
    end

    set wd (printf "%s$wd%s%s" (set_color $fish_color_cwd) (set_color normal) (__fish_vcs_prompt))

    # Include k8s context when enabled by 'kubeon'
    if test -n "$K8S_FISH_PROMPT"
        set -l kubernetes "k8s"
        if test -x (which kubectl)
            set kubernetes (printf "$kubernetes:%s" (kubectl config current-context))
        end
        if test -x (which kubens)
            set kubernetes (printf "$kubernetes:%s" (kubens -c))
        end
        set wd (printf "%s[$kubernetes]%s $wd" (set_color $fish_color_kubernetes) (set_color normal))
    end

    # The space before the '>' is needed due to aggresive ligatures
    set -l prompt_symbol (printf "%s>%s " (set_color $prompt_color) (set_color normal))

    printf "%s %s %s %s" $time $user $wd $prompt_symbol
end
