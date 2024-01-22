path+=("$HOME/.local/bin")

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# history
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_FIND_NO_DUPS
setopt HIST_SAVE_NO_DUPS

# completion
autoload -Uz compinit && compinit
setopt complete_in_word
setopt noautomenu
setopt nomenucomplete
setopt NO_CASE_GLOB
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# file system navigation
setopt autocd
eval "$(zoxide init --cmd j zsh)"

# line navigation
autoload -U select-word-style
select-word-style bash

PROMPT="%B%F{yellow}%~ λ%f%b "
bindkey \^u backward-kill-line

alias gs="git status"
alias gl="git log"
alias ga="git add"
alias gall="git add ."
alias gb="git branch -a"
alias gc="git commit -m"
alias gd="git diff"
alias gsh="git stash"

alias rs="repo status"
alias rsc="repo sync -c"
alias rd="repo diff"

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fd='fdfind'
alias rg="rg -i --hidden"
alias c='clear'
alias q='exit'

alias .1="cd .."
alias .2="cd ../.."
alias .3="cd ../../.."
alias .4="cd ../../../.."
alias .5="cd ../../../../.."
alias .6="cd ../../../../../.."
alias .7="cd ../../../../../../.."
alias .8="cd ../../../../../../../.."
alias .9="cd ../../../../../../../../.."
