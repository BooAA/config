[[ -o interactive ]] || return

if [[ -t 0 ]]; then
    stty -ixon
fi

setopt autocd
setopt hist_ignore_space
setopt hist_verify
setopt append_history
setopt extended_history

HISTFILE=~/.zsh_history
HISTSIZE=1000000
HISTSIZE=1000000
setopt SHARE_HISTORY

path+=(~/.local/bin)

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

autoload -Uz compinit
compinit

eval "$(zoxide init zsh --cmd j)"

PS1="%{$fg[yellow]%}%B%~%b λ %{$reset_color%}"

mkdir_cd() {
    mkdir $1 && cd $1
}

git_blame_line() {
    local l=$1 f=$2
    git blame -L $l,$l -- $f
}

git_log_line() {
    local l=$1 f=$2
    git log -L $l,$l:$f
}

up_dir() {
    local cnt=$1
    if [[ $cnt -le 0 ]]; then
      return
    fi

    for (( i=1; i<=$cnt; i++ )); do
        cd ..
    done
}

clear() {
    printf '\033[2J\033[3J\033[1;1H'
}

alias a='adb devices'
alias b='git branch'
alias bl='git_blame_line'
alias bls='git_log_line'
alias c='clear'
alias d='git diff'
alias e='emacsclient -q -t -a nvim'
alias f='fastboot devices'
alias h='history 0 | rg'
alias hn='hostname'
alias l='git log --oneline'
alias m='mkdir'
alias md='mkdir_cd'
alias mk='make -j$(nproc)'
alias p='git log -p'
alias q='exit'
alias s='git status'

alias ls='ls -G'
alias grep='grep --color=auto -i'
alias diff='diff --color=auto'

alias fd='fd -i --hidden'
alias rg='rg -i --hidden'

# alias vi='nvim'
# alias vim='nvim'

alias rs='repo status'
alias rsc='repo sync -c -j$(nproc)'
alias rd='repo diff'

alias gs='git status'
alias gl='git log --oneline'
alias ga='git add'
alias gall='git add .'
alias gb='git branch -a'
alias gc='git commit -s'
alias gd='git diff'
alias gdc='git diff --cached'
alias gsh='git stash'

for i in {1..10}; do
    alias .${i}="up_dir $i"
done
