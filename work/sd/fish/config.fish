function fish_greeting
	 # echo $fish_greeting
end

function old-sh
	 bash $argv
end

function sh
	 fish $argv
end

set -x VISUAL "emacs -nw "
set -x EDITOR "emacs -nw "
set PATH /usr/bin/ $PATH
set PATH C:/Io/bin C:/Io/lib $PATH
set PATH /cygdrive/c/Program\ Files\ \(x86\)/UiPath/Studio/ $PATH
set PATH /cygdrive/c/go/bin/ $PATH
set GOPATH $HOME/go

set PATH /home/sisto/perl5/bin $PATH
set PERL5LIB "/home/sisto/perl5/lib/perl5" $PERL5LIB
set PERL_LOCAL_LIB_ROOT /home/sisto/perl5 $PERL_LOCAL_LIB_ROOT
set PERL_MB_OPT "--install_base \"/home/sisto/perl5\""
set PERL_MM_OPT "INSTALL_BASE=/home/sisto/perl5"


set pink      ff99ff
set dark_pink cc99ff

# Fish git prompt
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

# start X at login
#if status --is-login
#    if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
#        exec startx
#    end
#end

