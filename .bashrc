case $- in
    *i*) ;;
    *) return;;
esac

case $(hostname) in
    liangjlee.c.googlers.com)
	PS1="\[\e[30;43m\]\w\[\e[0m\] \[\e[01;33m\]λ \[\e[0m\]"
	;;
    liangjlee2.c.googlers.com)
	PS1="\[\e[30;44m\]\w\[\e[0m\] \[\e[01;34m\]λ \[\e[0m\]"
	;;
    liangjlee3.c.googlers.com)
	PS1="\[\e[30;41m\]\w\[\e[0m\] \[\e[01;31m\]λ \[\e[0m\]"
	;;
    liangjlee4.c.googlers.com)
	PS1="\[\e[30;107m\]\w\[\e[0m\] \[\e[01;97m\]λ \[\e[0m\]"
	;;
    liangjlee-mail.c.googlers.com)
        PS1="\[\033[01;33m\]\w @ \[\e[0m\]"
        ;;
    liangjlee-carbonv9-linux)
        PS1="\[\033[01;33m\]\w λ \[\e[0m\]"
        ;;
    liangjlee-p620lin01.ntc.corp.google.com)
        PS1="\[\033[01;33m\]\w Λ \[\e[0m\]"
        ;;
esac

shopt -s checkwinsize
shopt -s globstar
shopt -s histappend
shopt -s autocd

HISTCONTROL=ignoreboth
HISTTIMEFORMAT="%F %T  "
HISTSIZE=10000000
HISTFILESIZE=10000000
PERSISTENT_HISTORY_LAST=''
#PATH=$PATH:~/.local/bin

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
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

if [[ -t 0 && $- = *i* ]]; then
    stty -ixon
fi 

function up_dir() {
    local cnt=$1
    while (( $cnt > 0 )); do
	cd ..
	((cnt=cnt-1))
    done
}

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

function log_bash_persistent_history()
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

PROMPT_COMMAND="log_bash_persistent_history; $PROMPT_COMMAND"
. "$HOME/.cargo/env"
