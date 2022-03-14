function fish_greeting
    # echo $fish_greeting
end

function sh
    fish $argv
end

set -x VISUAL "emacs "
set -x EDITOR "nano "

# Env
set -l system (uname)

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
set -x GUIX_PROFILE "$HOME/.guix-profile"
if test -d ~/.config/guix
    set -x PATH "$HOME/.config/guix/current/bin" $PATH
    set -x PATH "$GUIX_PROFILE/bin" $PATH
    set -x PATH "$GUIX_PROFILE/sbin" $PATH
    fenv source "$GUIX_PROFILE/etc/profile"
    fenv source "$HOME/.config/guix/current/etc/profile"
end

# Nix
if test -d ~/.nix-profile
    fenv source ~/.nix-profile/etc/profile.d/nix.sh
end

# Docker
set -x DOCKER_BUILDKIT 1

# fnm - cargo install fnm
if command -v fnm >/dev/null
    fnm env --shell=fish | source
end

# AWS
if command -v aws_completer >/dev/null
    complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
end

# At work?
if test "$USER" = "sst"
    # pyenv setup
    if test -d ~/.pyenv
        set -x PATH /home/sst/.pyenv/bin $PATH
        set -gx PATH '/home/sst/.pyenv/shims' $PATH
        set -gx PYENV_SHELL fish
        source '/home/sst/.pyenv/libexec/../completions/pyenv.fish'
        command pyenv rehash 2>/dev/null
        function pyenv
            set command $argv[1]
            set -e argv[1]
            switch "$command"
                case activate deactivate rehash shell
                    source (pyenv "sh-$command" $argv|psub)
                case '*'
                    command pyenv "$command" $argv
            end
        end
        set -gx PATH '/home/sst/.pyenv/plugins/pyenv-virtualenv/shims' $PATH;
        set -gx PYENV_VIRTUALENV_INIT 1;
        function _pyenv_virtualenv_hook --on-event fish_prompt;
            set -l ret $status
            if [ -n "$VIRTUAL_ENV" ]
                pyenv activate --quiet; or pyenv deactivate --quiet; or true
            else
                pyenv activate --quiet; or true
            end
            return $ret
        end
    end
    if command -v direnv >/dev/null
        direnv hook fish | source
    end
end

# XDG setup
set -x XDG_CONFIG_HOME "$HOME/.config"      # .dotfiles and configuration data
set -x XDG_DATA_HOME   "$HOME/.local/share" # local data, important

# opam configuration
source /home/sst/.config/opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# XCompose
set -x XCOMPOSEFILE "$HOME/.config/.XCompose"

# Colours
set pink      ff99ff
set dark_pink cc99ff

set orange    f2aa4c

set fish_color_nix $orange
set fish_color_kubernetes $dark_pink

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

# System specific setup
switch $system
    case CYGWIN_NT-10.0
        set -x PATH /usr/bin/ $PATH
end

# Start X at login
if test "$system" = "Linux"
    if status --is-login
        if test -z "$DISPLAY" -a "$XDG_VTNR" -eq 1
            exec startx
        end
    end
end
