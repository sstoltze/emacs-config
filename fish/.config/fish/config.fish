function fish_greeting
	 # echo $fish_greeting
end

function sh
	 fish $argv
end

set -x VISUAL "emacs -nw "
set -x EDITOR "emacs -nw "

# Env

# Stack/pip/...
if test -d ~/.local/bin
    set -x PATH ~/.local/bin $PATH
end

# Cabal
if test -d ~/.cabal/bin
    set -x PATH ~/.cabal/bin $PATH
end

# Mitmproxy keylogs
if test -d ~/.config/mitmproxy
    set -x MITM_SSLKEYLOGFILE ~/.config/mitmproxy/sslkeylogs.log
end

# Rust
set -x CARGO_HOME  "$HOME/.local"
set -x RUSTUP_HOME "$HOME/.local/rustup"

# Guix
set -x GUIX_LOCPATH "$HOME/.guix-profile/lib/locale"
if test -d ~/.config/guix
    set -x PATH ~/.config/guix/current/bin $PATH
    set -x PATH ~/.guix-profile/bin $PATH
end

# System specific setup
set -l system (uname)

switch $system
    case CYGWIN_NT-10.0
        set -x PATH /usr/bin/ $PATH
    case Linux
        #
        # start X at login
        if status --is-login
            if test -z "$DISPLAY" -a "$XDG_VTNR" -eq 1
                exec startx
            end
        end
end

# At work?
if test "$USER" = "w26164"
    direnv hook fish | source
    set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
    set -x FLAMEGRAPH_DIR ~/git/FlameGraph
end

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
set __fish_git_prompt_showuntrackedfiles      'yes'
#set __fish_git_prompt_color_upstream_ahead   green
#set __fish_git_prompt_color_upstream_behind  red
#set __fish_git_prompt_color_branch           $dark_pink
set __fish_git_prompt_color_branch      -o yellow
set __fish_git_prompt_color_dirtystate  -o red
set __fish_git_prompt_color_stagedstate -o blue
set __fish_git_prompt_color                cyan
# Git status
set __fish_git_prompt_char_
set __fish_git_prompt_char_dirtystate      '+'
set __fish_git_prompt_char_stagedstate     '→'
set __fish_git_prompt_char_stashstate      '↩'
