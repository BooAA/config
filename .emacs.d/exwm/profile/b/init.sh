#!/bin/sh

xrandr --newmode profile_b 60 3840 0 0 0 2160 0 0 0
xrandr --addmode DUMMY0 profile_b
