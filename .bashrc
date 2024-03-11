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

up_dir() {
    local cnt=$1
    while (( $cnt > 0 )); do
	cd ..
	((cnt=cnt-1))
    done
}

alias a='adb devices'
alias b='git branch'
alias bl='git_blame_line'
alias bls='git_log_line'
alias c='clear'
alias d='git diff'
alias e='emacsclient -q -t -a nvim'
alias f='fdfind -i --hidden'
alias g='rg -i --hidden'
alias h='cat ~/.persistent_history | rg '
alias l='git log --oneline'
alias m='make -j$(nproc)'
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
alias grep='grep --color=auto'

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

for i in {1..10}; do
    alias .${i}="up_dir $i"
done

declare -A pkgs=(
    # first party apps
    ["dialer"]="com.google.android.dialer"
    ["youtube"]="com.google.android.youtube"
    ["map"]="com.google.android.apps.maps"
    ["chrome"]="com.android.chrome"
    ["gmail"]="com.google.android.gm"
    ["calendar"]="com.google.android.calendar"
    ["message"]="com.google.android.apps.messaging"
    ["photo"]="com.google.android.apps.photos"
    ["deskclock"]="com.google.android.deskclock"
    ["camera"]="com.google.android.GoogleCamera"

    # third-party apps
    ['facebook']="com.facebook.katana"
    ['instagram']="com.instagram.android"
    ['messenger']="com.facebook.orca"
    ['spotify']="com.spotify.music"
    ['twitter']="com.twitter.android"
    ['reddit']="com.reddit.frontpage"
    ['amazon']="com.amazon.mShop.android.shopping"
    ['twitch']="tv.twitch.android.app"
    ['CNN']="com.cnn.mobile.android.phone"
    ['tiktok']="com.ss.android.ugc.trill"
    ['whatsapp']="com.whatsapp"

    # dummy apps to make others cached
    ["calculator"]="com.google.android.calculator"
)

declare -A intents=(
    # first party apps
    ["dialer"]="com.google.android.dialer/.extensions.GoogleDialtactsActivity"
    ["youtube"]="com.google.android.youtube/com.google.android.apps.youtube.app.watchwhile.WatchWhileActivity"
    ["map"]="com.google.android.apps.maps/com.google.android.maps.MapsActivity"
    ["chrome"]="com.android.chrome/com.google.android.apps.chrome.Main"
    ["gmail"]="com.google.android.gm/.ConversationListActivityGmail"
    ["calendar"]="com.google.android.calendar/com.android.calendar.AllInOneActivity"
    ["message"]="com.google.android.apps.messaging/com.google.android.apps.messaging.main.MainActivity"
    ["photo"]="com.google.android.apps.photos/.home.HomeActivity"
    ["deskclock"]="com.google.android.deskclock/com.android.deskclock.DeskClock"
    ["camera"]="com.google.android.GoogleCamera/com.android.camera.CameraLauncher"

    # third-party apps
    ["facebook"]="com.facebook.katana/.activity.FbMainTabActivity"
    ["instagram"]="com.instagram.android/com.instagram.mainactivity.MainActivity"
    ["messenger"]="com.facebook.orca/com.facebook.messenger.neue.MainActivity"
    ["spotify"]="com.spotify.music/.MainActivity"
    ["twitter"]="com.twitter.android/com.twitter.app.main.MainActivity"
    ["reddit"]="com.reddit.frontpage/com.reddit.launch.main.MainActivity"
    ['amazon']="com.amazon.mShop.android.shopping/com.amazon.mShop.navigation.MainActivity"
    ['twitch']="tv.twitch.android.app/tv.twitch.android.feature.viewer.main.MainActivity"
    ['CNN']="com.cnn.mobile.android.phone/com.cnn.mobile.android.phone.features.main.MainActivity"
    ['tiktok']="com.ss.android.ugc.trill/com.ss.android.ugc.aweme.splash.SplashActivity"
    ['whatsapp']="com.whatsapp/com.whatsapp.HomeActivity"

    # dummy apps to make others cached
    ["calculator"]="com.google.android.calculator/com.android.calculator2.Calculator"
)
