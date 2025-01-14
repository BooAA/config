#!/bin/sh

xrandr --newmode profile_a 60 3840 0 0 0 2400 0 0 0
xrandr --addmode DUMMY0 profile_a
