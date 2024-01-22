(in-package :stumpwm)

(setf *shell-program* "/usr/bin/bash")

(setf *mode-line-timeout* 2
      *time-modeline-string* "%a %b %e %k:%M"
      *screen-mode-line-format* '("[%n] %W ^> %l %P [%B] [%d]"))

(setf pamixer:*step* 2
      pamixer:*allow-boost* t
      pamixer:*source-allow-boost* t)

(setf notify:*notify-server-title-color* "^3"
      notify:*notify-server-body-color* "^7")

(notify:notify-server-on)

(run-commands
 "gnewbg-dynamic Term"
 "gnewbg Emacs"
 "gnewbg Misc")

(run-shell-command "feh --bg-scale ~/Pictures/wallpapers/blackhole.jpg")
(run-shell-command "picom")
(run-shell-command "fcitx5")
