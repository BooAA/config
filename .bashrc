case $- in
    *i*) ;;
    *) return;;
esac

if [[ -t 0 && $- = *i* ]]; then
    stty -ixon
fi 

if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

if [ -f $HOME/Src/LS_COLORS/lscolors.sh ]; then
    . $HOME/Src/LS_COLORS/lscolors.sh
fi

if command -v zoxide &> /dev/null; then
    eval "$(zoxide init --cmd j bash)"
fi

shopt -s autocd
shopt -s checkwinsize
shopt -s globstar
shopt -s histappend

HISTCONTROL=ignoreboth
HISTFILESIZE=100000
HISTSIZE=100000
HISTTIMEFORMAT="%F %T  "
PATH=$PATH:~/.local/bin
PERSISTENT_HISTORY_LAST=''
PROMPT_COMMAND="log_bash_persistent_history; $PROMPT_COMMAND"
PS1="\[\033[01;33m\]\w λ \[\e[0m\]"

XDG_CONFIG_HOME="$HOME/.config"
XDG_CACHE_HOME="$HOME/.cache"
XDG_DATA_HOME="$HOME/.local/share"
XDG_STATE_HOME="$HOME/.local/state"

mkdir_cd() {
    mkdir $1; cd $1
}

git_blame_line() {
    local l=$1 f=$2
    git blame -L $l,$l -- $f
}

git_log_line() {
    local l=$1 f=$2
    git log -L $l,$l:$f
}

log_bash_persistent_history()
{
    [[
        $(history 1) =~ ^\ *[0-9]+\ +([^\ ]+\ [^\ ]+)\ +(.*)$
    ]]
    local date_part="${BASH_REMATCH[1]}"
    local command_part="${BASH_REMATCH[2]}"
    if [ "$command_part" != "$PERSISTENT_HISTORY_LAST" ]
    then
        echo $date_part "|" "$command_part" >> ~/.persistent_history
        export PERSISTENT_HISTORY_LAST="$command_part"
    fi
}

vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

emacs_comint_prompt () {
    printf "\e]7;file://%s%s\e\\" "$HOSTNAME" "$PWD"
}

case $INSIDE_EMACS in
    "vterm")
        PS1=$PS1'\[$(vterm_prompt_end)\]'
        ;;
    "32.0.50,comint"|"31.0.50,comint")
        PROMPT_COMMAND="emacs_comint_prompt; $PROMPT_COMMAND"
        ;;
    *)
        ;;
esac

up_dir() {
    local cnt=$1
    while (( $cnt > 0 )); do
	cd ..
	((cnt=cnt-1))
    done
}

clear() {
    printf '\033[2J\033[3J\033[1;1H'
}

booaa_setup() {
    local config=$1
    ln -s $config/.bashrc ~/.bashrc
    ln -s $config/.inputrc ~/.inputrc
    ln -s $config/alacritty ~/.config/alacritty

    mkdir ~/.config/tmux
    ln -s $config/tmux/tmux.conf ~/.config/tmux/tmux.conf

    mkdir ~/.emacs.d
    ln -s $config/.emacs.d/init.el ~/.emacs.d/init.el
    ln -s $config/.emacs.d/early-init.el ~/.emacs.d/early-init.el
    ln -s $config/.emacs.d/exwm ~/.emacs.d/exwm
    ln -s $config/.emacs.d/site-lisp ~/.emacs.d/site-lisp
    ln -s $config/.emacs.d/tree-sitter ~/.emacs.d/tree-sitter
}

booaa_reset() {
    rm ~/.bashrc ~/.inputrc
    rm -rf ~/.config/alacritty ~/.config/tmux/ ~/.emacs.d
}

alias a='adb devices'
alias ar='adb root'
alias as='adb shell'
alias b='git branch'
alias bl='git_blame_line'
alias bls='git_log_line'
alias c='clear'
alias d='git diff'
alias e='emacsclient -q -t -a nvim'
alias f='fastboot devices'
alias h='cat ~/.persistent_history | rg'
alias k='kill -9'
alias l='git log --oneline'
alias m='mkdir'
alias md='mkdir_cd'
alias mk='make -j$(nproc)'
alias p='git log -p'
alias pg='ps aux | rg'
alias pk='pkill'
alias q='exit'
alias s='git status'
alias v='nvim'

alias diff='diff --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto -i'
if command -v fdfind &> /dev/null; then alias fd='fdfind -i --hidden'; fi
alias rg='rg -i --hidden'

alias vi='nvim'
alias vim='nvim'
alias v2='nvim -O2'

alias gs='git status'
alias gl='git log --oneline'
alias ga='git add'
alias gall='git add .'
alias gb='git branch -a'
alias gc='git commit -s'
alias gd='git diff'
alias gdc='git diff --cached'
alias gsh='git stash'

alias rs='repo status'
alias rsc='repo sync -c -j $(nproc)'
alias rd='repo diff'

alias hn='hostname'

for i in {1..10}; do
    alias .${i}="up_dir $i"
done

if [ -f "$HOME/.bashrc_private" ]; then
    . "$HOME/.bashrc_private"
fi
