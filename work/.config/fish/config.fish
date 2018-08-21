function fish_greeting
	 # echo $fish_greeting
end

function sh
	 fish $argv
end

set -x VISUAL "emacs -nw "
set -x EDITOR "emacs -nw "

set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_DATA_HOME "$HOME/.local/share"

set pink      ff99ff
set dark_pink cc99ff

# Git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_show_informative_status 'yes'
#set __fish_git_prompt_showuntrackedfiles 'yes'
#set __fish_git_prompt_color_upstream_ahead green
#set __fish_git_prompt_color_upstream_behind red
#set __fish_git_prompt_color_branch         $dark_pink
set __fish_git_prompt_color_branch      -o yellow
set __fish_git_prompt_color_dirtystate  -o red
set __fish_git_prompt_color_stagedstate -o blue
set __fish_git_prompt_color                cyan
# Status Chars
set __fish_git_prompt_char_dirtystate      '+'
set __fish_git_prompt_char_stagedstate     '→'
set __fish_git_prompt_char_stashstate      '↩'

# System specific setup
set -l system (uname)

switch $system
    case CYGWIN_NT-10.0
        set -x PATH /usr/bin/ $PATH

        set -x PATH /cygdrive/c/Program\ Files\ \(x86\)/UiPath/Studio/ $PATH

        #if test -d /cygdrive/c/Io
        #    set -x PATH /cygdrive/c/Io/bin /cygdrive/c/Io/lib $PATH
        #end

    case Linux
        set -x PATH ~/.cabal/bin $PATH

        # start X at login
        if status --is-login
            if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
                exec startx
            end
        end
end
