#!/bin/sh

echo "configure keyboard layout with xmodmap"
setxkbmap
xmodmap .Xmodmap

echo "update xrandr to profile_b"
xrandr --output DUMMY0 --mode profile_b

echo "update X resources"
xrdb -merge .Xresources

dpi=$(grep Xft.dpi .Xresources | cut -f2 -d' ')
echo "set emacs dpi to $dpi"
emacsclient --eval "(internal-set-lisp-face-attribute 'default :font (font-spec :size 9.0 :dpi $dpi) 0)"

echo "cleanup chrome process with old dpi"
kill -9 $(pgrep -a chrome | grep -v chrome-remote-desktop | awk '{print $1}')
