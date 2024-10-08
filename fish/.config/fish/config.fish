function fish_greeting
    # echo $fish_greeting
end

function sh
    fish $argv
end

set -x VISUAL "emacs"
set -x EDITOR "emacs -nw"

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
set -x GUIX_PROFILE "$HOME/.guix-profile"
set -x GUIX_LOCPATH "$GUIX_PROFILE/lib/locale"
set -x PATH "$GUIX_PROFILE/bin" $PATH
set -x PATH "$GUIX_PROFILE/sbin" $PATH
if test -d "$GUIX_PROFILE/etc/profile"
    fenv source "$GUIX_PROFILE/etc/profile"
end
if test -d ~/.config/guix
    # The order here is important, as the most recent guix is in "$HOME/.config/guix/current/bin"
    # So this needs to be run as the last update of path
    fenv source "$HOME/.config/guix/current/etc/profile"
    set -x PATH "$HOME/.config/guix/current/bin" $PATH
end

# Nix
if test -d ~/.nix-profile -a -e ~/.nix-profile/etc/profile.d/nix.sh
    fenv source ~/.nix-profile/etc/profile.d/nix.sh
else if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
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

if command -v direnv >/dev/null
    direnv hook fish | source
end

# XDG setup
set -x XDG_CONFIG_HOME "$HOME/.config"      # .dotfiles and configuration data
set -x XDG_DATA_HOME   "$HOME/.local/share" # local data, important

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
    case Darwin
        # M-b
        bind \u222B backward-bigword
        # M-f
        bind \u0192 forward-bigword
        # M-d
        bind \u2202 kill-bigword
end

# Start X at login
if test "$system" = "Linux"
    if status --is-login
        if test -z "$DISPLAY" -a "$XDG_VTNR" -eq 1
            exec startx
        end
    end
end

set -x GIT_MOB_COAUTHORS "$HOME/.git_coauthors.json"

if command -v brew >/dev/null
    set -gx HOMEBREW_PREFIX "/opt/homebrew";
    set -gx HOMEBREW_CELLAR "/opt/homebrew/Cellar";
    set -gx HOMEBREW_REPOSITORY "/opt/homebrew";
    set -q PATH; or set PATH ''; set -gx PATH "/opt/homebrew/bin" "/opt/homebrew/sbin" $PATH;
    set -q MANPATH; or set MANPATH ''; set -gx MANPATH "/opt/homebrew/share/man" $MANPATH;
    set -q INFOPATH; or set INFOPATH ''; set -gx INFOPATH "/opt/homebrew/share/info" $INFOPATH;
end
