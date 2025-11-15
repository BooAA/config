(in-package :stumpwm)

(load "~/.stumpwm.d/.qlot/setup.lisp")

(ql:quickload "clx-truetype")
(ql:quickload "dbus")
(ql:quickload "slynk")
(ql:quickload "xml-emitter")
(ql:quickload "zpng")

(set-module-dir "~/.stumpwm.d/modules")

(load-module "battery-portable")
(load-module "command-history")
(load-module "globalwindows")
(load-module "net")
(load-module "notify")
(load-module "pamixer")
(load-module "screenshot")
(load-module "shell-command-history")
(load-module "swm-gaps")
(load-module "ttf-fonts")

(set-prefix-key (kbd "H-x"))
(setf *float-window-modifier* :hyper)

(define-key *root-map* (kbd "0") "remove-split")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")
(define-key *root-map* (kbd "o") "fnext")
(define-key *root-map* (kbd "O") "fprev")
(define-key *root-map* (kbd "^") "iresize")
(define-key *root-map* (kbd "x") '*exchange-window-map*)
(define-key *root-map* (kbd "w") "fselect")
(define-key *root-map* (kbd ";") "colon")
(define-key *root-map* (kbd ":") "eval")
(define-key *root-map* (kbd "!") "exec")
(define-key *root-map* (kbd "+") "balance-frames")
(define-key *root-map* (kbd "b") "pull-from-windowlist")
(define-key *root-map* (kbd "H-b") "windowlist")
(define-key *root-map* (kbd "B") "global-pull-windowlist")
(define-key *root-map* (kbd "s-B") "global-windowlist")
(define-key *root-map* (kbd "k") "delete")
(define-key *root-map* (kbd "#") "mark")
(define-key *root-map* (kbd "Left") "prev")
(define-key *root-map* (kbd "Right") "next")

(define-key *root-map* (kbd "H-0") "select-window-by-number 0")
(define-key *root-map* (kbd "H-1") "select-window-by-number 1")
(define-key *root-map* (kbd "H-2") "select-window-by-number 2")
(define-key *root-map* (kbd "H-3") "select-window-by-number 3")
(define-key *root-map* (kbd "H-4") "select-window-by-number 4")
(define-key *root-map* (kbd "H-5") "select-window-by-number 5")
(define-key *root-map* (kbd "H-6") "select-window-by-number 6")
(define-key *root-map* (kbd "H-7") "select-window-by-number 7")
(define-key *root-map* (kbd "H-8") "select-window-by-number 8")
(define-key *root-map* (kbd "H-9") "select-window-by-number 9")

(define-key *root-map* (kbd "H-)") "pull 0")
(define-key *root-map* (kbd "H-!") "pull 1")
(define-key *root-map* (kbd "H-@") "pull 2")
(define-key *root-map* (kbd "H-#") "pull 3")
(define-key *root-map* (kbd "H-$") "pull 4")
(define-key *root-map* (kbd "H-%") "pull 5")
(define-key *root-map* (kbd "H-^") "pull 6")
(define-key *root-map* (kbd "H-&") "pull 7")
(define-key *root-map* (kbd "H-*") "pull 8")
(define-key *root-map* (kbd "H-(") "pull 9")

(define-key *root-map* (kbd "H-c") "quit-confirm")
(define-key *root-map* (kbd "H-l") "exec dm-tool lock")
(define-key *root-map* (kbd "H-z") "exec systemctl suspend")
(define-key *root-map* (kbd "H-q") "meta")

(define-key *exchange-window-map* (kbd "r") "title")

(define-key *root-map* (kbd "t") '*groups-map*)
(define-key *groups-map* (kbd "0") "gkill")
(define-key *groups-map* (kbd "1") "gkill-other")
(define-key *groups-map* (kbd "2") "gnew")
(define-key *groups-map* (kbd "o") "gnext")
(define-key *groups-map* (kbd "O") "gprev")
(define-key *groups-map* (kbd "m") "gnext-with-window")
(define-key *groups-map* (kbd "M") "gprev-with-window")
(define-key *groups-map* (kbd "r") "grename")
(define-key *groups-map* (kbd "RET") "gselect")

(define-key *top-map* (kbd "H-1") "gselect 1")
(define-key *top-map* (kbd "H-2") "gselect 2")
(define-key *top-map* (kbd "H-3") "gselect 3")
(define-key *top-map* (kbd "H-4") "gselect 4")
(define-key *top-map* (kbd "H-5") "gselect 5")
(define-key *top-map* (kbd "H-6") "gselect 6")
(define-key *top-map* (kbd "H-7") "gselect 7")
(define-key *top-map* (kbd "H-8") "gselect 8")
(define-key *top-map* (kbd "H-9") "gselect 9")

(define-key *top-map* (kbd "H-!") "gmove 1")
(define-key *top-map* (kbd "H-@") "gmove 2")
(define-key *top-map* (kbd "H-#") "gmove 3")
(define-key *top-map* (kbd "H-$") "gmove 4")
(define-key *top-map* (kbd "H-%") "gmove 5")
(define-key *top-map* (kbd "H-^") "gmove 6")
(define-key *top-map* (kbd "H-&") "gmove 7")
(define-key *top-map* (kbd "H-*") "gmove 8")
(define-key *top-map* (kbd "H-(") "gmove 9")

(define-key *top-map* (kbd "H-C-1") "gmove-and-follow 1")
(define-key *top-map* (kbd "H-C-2") "gmove-and-follow 2")
(define-key *top-map* (kbd "H-C-3") "gmove-and-follow 3")
(define-key *top-map* (kbd "H-C-4") "gmove-and-follow 4")
(define-key *top-map* (kbd "H-C-5") "gmove-and-follow 5")
(define-key *top-map* (kbd "H-C-6") "gmove-and-follow 6")
(define-key *top-map* (kbd "H-C-7") "gmove-and-follow 7")
(define-key *top-map* (kbd "H-C-8") "gmove-and-follow 8")
(define-key *top-map* (kbd "H-C-9") "gmove-and-follow 9")

(define-key *top-map* (kbd "H-Up") "move-focus up")
(define-key *top-map* (kbd "H-Down") "move-focus down")
(define-key *top-map* (kbd "H-Left") "move-focus left")
(define-key *top-map* (kbd "H-Right") "move-focus right")

(define-key *top-map* (kbd "H-S-Up") "move-window up")
(define-key *top-map* (kbd "H-S-Down") "move-window down")
(define-key *top-map* (kbd "H-S-Left") "move-window left")
(define-key *top-map* (kbd "H-S-Right") "move-window right")

(define-key *top-map* (kbd "H-h") '*help-map*)
(define-key *help-map* (kbd "v") "describe-variable")
(define-key *help-map* (kbd "f") "describe-function")
(define-key *help-map* (kbd "k") "describe-key")
(define-key *help-map* (kbd "b") "where-is")
(define-key *help-map* (kbd "x") "describe-command")
(define-key *help-map* (kbd "e") "lastmsg")
(define-key *help-map* (kbd "E") "copy-last-message")
(define-key *help-map* (kbd "i") "list-window-properties")

(define-key *top-map* (kbd "H-;") "colon")
(define-key *top-map* (kbd "H-s") "exec rofi -show combi")
(define-key *top-map* (kbd "H-:") "eval")
(define-key *top-map* (kbd "H-RET") "exec alacritty")
(define-key *top-map* (kbd "Print") "screenshot-area")
(define-key *top-map* (kbd "H-Print") "screenshot-window")
(define-key *top-map* (kbd "H-Sys_Req") "screenshot")

(define-key *top-map* (kbd "H-w") "exec jumpapp google-chrome")
(define-key *top-map* (kbd "H-t") "exec jumpapp alacritty")
(define-key *top-map* (kbd "H-e") "exec jumpapp emacs")
(define-key *top-map* (kbd "H-f") "exec jumpapp thunar")
(define-key *top-map* (kbd "H-v") "exec jumpapp pavucontrol")
(define-key *top-map* (kbd "H-c") "exec jumpapp telegram-desktop")
(define-key *top-map* (kbd "H-b") "exec jumpapp blueman-manager")

(define-key *top-map* (kbd "H-y") "mode-line")
(define-key *top-map* (kbd "H-d") "fclear")
(define-key *top-map* (kbd "H-g") "lastmsg")
(define-key *top-map* (kbd "H-G") "copy-last-message")
(define-key *top-map* (kbd "H-u") "vsplit")
(define-key *top-map* (kbd "H-i") "hsplit")
(define-key *top-map* (kbd "H-o") "fnext")
(define-key *top-map* (kbd "H-O") "fprev")
(define-key *top-map* (kbd "H-m") "only")
(define-key *top-map* (kbd "H-r") "exec")
(define-key *top-map* (kbd "H-j") "fnext")
(define-key *top-map* (kbd "H-k") "fprev")
(define-key *top-map* (kbd "H-l") "curframe")
(define-key *top-map* (kbd "H-q") "remove-split")
(define-key *top-map* (kbd "H-SPC") "float-this")
(define-key *top-map* (kbd "H-S-SPC") "unfloat-this")
(define-key *top-map* (kbd "H-TAB") "next")
(define-key *top-map* (kbd "H-ISO_Left_Tab") "prev")

(define-key *top-map* (kbd "H-F1") "pamixer-toggle-mute")
(define-key *top-map* (kbd "H-F2") "pamixer-volume-down")
(define-key *top-map* (kbd "H-F3") "pamixer-volume-up")
(define-key *top-map* (kbd "H-F10") "fullscreen")

(define-key *top-map* (kbd "H-S-F1") "pamixer-source-toggle-mute")
(define-key *top-map* (kbd "H-S-F2") "pamixer-source-volume-down")
(define-key *top-map* (kbd "H-S-F3") "pamixer-source-volume-up")

(set-bg-color "#3F3F3F")
(set-fg-color "#DCDCCC")
(set-focus-color "#9FC59F")
(set-unfocus-color "#5F7F5F")

(let ((font-cache (merge-pathnames ".fonts/font-cache.sexp"
                                   (user-homedir-pathname))))
  (when (not (probe-file font-cache))
    (clx-truetype::cache-fonts)))

(let* ((scale 2)
       (noto-cjk (ignore-errors (make-instance 'xft:font
                                               :family "Noto Sans Mono CJK TC"
                                               :subfamily "Book"
                                               :size (* 10 scale)
                                               :antialias t)))
       (hack (ignore-errors (make-instance 'xft:font
                                           :family "Hack"
                                           :subfamily "Regular"
                                           :size (* 10 scale)
                                           :antialias t)))
       (fallback (format nil "-misc-fixed-medium-r-*--~a-*-*-*-*-*-*-*" (* 12 scale))))
  (set-font (remove nil (list noto-cjk hack fallback))))

(setf *input-window-gravity* :top
      *message-window-gravity* :top
      *message-window-input-gravity* :top-right
      *window-border-style* :none
      *message-window-padding* 5)

(setf *mouse-focus-policy* :click)

(setf *input-completion-style* (make-input-completion-style-unambiguous))

(setf swm-gaps:*head-gaps-size* 2
      swm-gaps:*inner-gaps-size* 2
      swm-gaps:*outer-gaps-size* 2)

(swm-gaps:toggle-gaps-on)

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
(run-shell-command "echo \"Xft.dpi: 192\" | xrdb -merge -")
(run-shell-command "xinput set-prop \"ELAN0672:00 04F3:3187 Touchpad\" \"libinput Tapping Enabled\" 1")
(run-shell-command "xinput set-prop \"ELAN0672:00 04F3:3187 Touchpad\" \"libinput Accel Speed\" 0.5")
(run-shell-command "xinput set-prop \"ELAN0672:00 04F3:3187 Touchpad\" \"libinput Natural Scrolling Enabled\" 1")
(run-shell-command "xset r rate 190 75")
