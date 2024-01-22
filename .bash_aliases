alias ls='ls --color=auto'
alias ll='ls -al'
alias l='ls -a'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'

alias fd='fdfind -i --hidden'
alias rg='rg -i --hidden'

alias a='adb devices'
alias b='git branch'
alias c='clear'
alias d='git diff'
alias e='emacsclient -q -t -a nvim'
alias f='fdfind -i --hidden'
alias g='rg -i --hidden'
alias h='cat ~/.persistent_history | rg '
alias m='make -j$(nproc)'
alias q='exit'
alias s='git status'
alias z='zellij'

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

for i in {1..10}; do
    alias .${i}="up_dir $i"
done
