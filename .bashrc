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
    "31.0.50,comint")
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
alias g='rg -i --hidden'
alias h='cat ~/.persistent_history | rg'
alias l='git log --oneline'
alias m='mkdir'
alias md='mkdir_cd'
alias mk='make -j$(nproc)'
alias p='git log -p'
alias pg='ps aux | rg'
alias q='exit'
alias s='git status'
alias z='tmux'
alias za='tmux attach'
alias zl='tmux ls'
alias zk='screen -X quit -S'
alias zK='screen -wipe'
alias zz='screen -c ~/.screenrc_detach'

alias diff='diff --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto -i'

alias fd='fdfind -i --hidden'
alias rg='rg -i --hidden'

alias vi='nvim'
alias vim='nvim'

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
