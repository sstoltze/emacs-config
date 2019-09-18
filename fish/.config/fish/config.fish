function fish_greeting
	 # echo $fish_greeting
end

function sh
	 fish $argv
end

set -x VISUAL "emacs -nw "
set -x EDITOR "emacs -nw "

# XDG setup
#set -x XDG_CONFIG_HOME "$HOME/.config"      # .dotfiles and configuration data
#set -x XDG_DATA_HOME   "$HOME/.local/share" # local data, important

# Colours
set pink      ff99ff
set dark_pink cc99ff

# Git prompt
set __fish_git_prompt_showdirtystate          'yes'
set __fish_git_prompt_showstashstate          'yes'
set __fish_git_prompt_showupstream            'yes'
set __fish_git_prompt_show_informative_status 'yes'
#set __fish_git_prompt_showuntrackedfiles     'yes'
#set __fish_git_prompt_color_upstream_ahead   green
#set __fish_git_prompt_color_upstream_behind  red
#set __fish_git_prompt_color_branch           $dark_pink
set __fish_git_prompt_color_branch      -o yellow
set __fish_git_prompt_color_dirtystate  -o red
set __fish_git_prompt_color_stagedstate -o blue
set __fish_git_prompt_color                cyan
# Git status
set __fish_git_prompt_char_dirtystate      '+'
set __fish_git_prompt_char_stagedstate     '→'
set __fish_git_prompt_char_stashstate      '↩'

# At work?
if test "$USER" = "w26164"
    direnv hook fish | source
    set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
    set -x FLAMEGRAPH_DIR ~/git/FlameGraph
end

# System specific setup
set -l system (uname)

switch $system
    case CYGWIN_NT-10.0
        set -x PATH /usr/bin/ $PATH

        # UiPath
        #set -x PATH /cygdrive/c/Program\ Files\ \(x86\)/UiPath/Studio/ $PATH

        # Io
        #if test -d /cygdrive/c/Io
        #    set -x PATH /cygdrive/c/Io/bin /cygdrive/c/Io/lib $PATH
        #end

    case Linux
        # Cabal
        if test -d ~/.cabal/bin
            set -x PATH ~/.cabal/bin $PATH
        end
        # Stack/pip/...
        if test -d ~/.local/bin
            set -x PATH ~/.local/bin $PATH
        end

        # start X at login
        if status --is-login
            if test -z "$DISPLAY" -a "$XDG_VTNR" -eq 1
                exec startx
            end
        end
end
