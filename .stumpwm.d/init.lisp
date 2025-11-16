(in-package :stumpwm)

(let ((quicklisp-init (merge-pathnames ".qlot/setup.lisp" *data-dir*)))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "clx-truetype")
(ql:quickload "dbus")
(ql:quickload "quri")
(ql:quickload "slynk")
(ql:quickload "xml-emitter")
(ql:quickload "zpng")

(set-module-dir "~/.stumpwm.d/modules")

(load-module "battery-portable")
(load-module "command-history")
(load-module "globalwindows")
(load-module "lookup")
(load-module "net")
(load-module "notify")
(load-module "pamixer")
(load-module "screenshot")
(load-module "shell-command-history")
(load-module "swm-gaps")
(load-module "ttf-fonts")

(defcommand chrome () ()
  (run-or-raise "google-chrome" '(:class "Google-chrome")))

(defcommand tmux () ()
  (run-or-raise
   "alacritty -T tmux --config-file /home/liangjlee/.config/alacritty/alacritty_tmux.toml -e tmux"
   '(:class "Alacritty"
     :title "tmux")))

(defcommand nautilus () ()
  (run-or-raise "nautilus" '(:class "nautilus")))

(defcommand pavucontrol () ()
  (run-or-raise "pavucontrol" '(:class "pavucontrol")))

(defcommand blueman-manager () ()
  (run-or-raise "blueman-manager" '(:class "blueman-manager")))

(set-prefix-key (kbd "H-x"))
(setf *float-window-modifier* :hyper)

(define-key *root-map* (kbd "0") "remove-split")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")
(define-key *root-map* (kbd "b") "global-pull-windowlist")
(define-key *root-map* (kbd "k") "delete")
(define-key *root-map* (kbd "o") "fnext")
(define-key *root-map* (kbd "O") "fprev")
(define-key *root-map* (kbd "w") "fselect")
(define-key *root-map* (kbd "^") "iresize")
(define-key *root-map* (kbd ";") "colon")
(define-key *root-map* (kbd ":") "eval")
(define-key *root-map* (kbd "!") "exec")
(define-key *root-map* (kbd "+") "balance-frames")
(define-key *root-map* (kbd "#") "mark")
(define-key *root-map* (kbd "Left") "prev")
(define-key *root-map* (kbd "Right") "next")

(define-key *root-map* (kbd "H-b") "global-windowlist")
(define-key *root-map* (kbd "H-c") "quit-confirm")
(define-key *root-map* (kbd "H-l") "exec dm-tool lock")
(define-key *root-map* (kbd "H-z") "exec systemctl suspend")
(define-key *root-map* (kbd "H-q") "meta")

(define-key *root-map* (kbd "x") '*exchange-window-map*)
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

(define-key *top-map* (kbd "H-?") '*help-map*)
(define-key *help-map* (kbd "v") "describe-variable")
(define-key *help-map* (kbd "f") "describe-function")
(define-key *help-map* (kbd "k") "describe-key")
(define-key *help-map* (kbd "b") "where-is")
(define-key *help-map* (kbd "x") "describe-command")
(define-key *help-map* (kbd "e") "lastmsg")
(define-key *help-map* (kbd "E") "copy-last-message")
(define-key *help-map* (kbd "i") "list-window-properties")

(define-key *top-map* (kbd "H-1") "gselect 1")
(define-key *top-map* (kbd "H-2") "gselect 2")
(define-key *top-map* (kbd "H-3") "gselect 3")
(define-key *top-map* (kbd "H-4") "gselect 4")
(define-key *top-map* (kbd "H-5") "gselect 5")
(define-key *top-map* (kbd "H-6") "gselect 6")
(define-key *top-map* (kbd "H-7") "gselect 7")
(define-key *top-map* (kbd "H-8") "gselect 8")
(define-key *top-map* (kbd "H-9") "gselect 9")

(define-key *top-map* (kbd "H-!") "gmove-and-follow 1")
(define-key *top-map* (kbd "H-@") "gmove-and-follow 2")
(define-key *top-map* (kbd "H-#") "gmove-and-follow 3")
(define-key *top-map* (kbd "H-$") "gmove-and-follow 4")
(define-key *top-map* (kbd "H-%") "gmove-and-follow 5")
(define-key *top-map* (kbd "H-^") "gmove-and-follow 6")
(define-key *top-map* (kbd "H-&") "gmove-and-follow 7")
(define-key *top-map* (kbd "H-*") "gmove-and-follow 8")
(define-key *top-map* (kbd "H-(") "gmove-and-follow 9")

(define-key *top-map* (kbd "H-k") "move-focus up")
(define-key *top-map* (kbd "H-j") "move-focus down")
(define-key *top-map* (kbd "H-h") "move-focus left")
(define-key *top-map* (kbd "H-l") "move-focus right")

(define-key *top-map* (kbd "H-K") "move-window up")
(define-key *top-map* (kbd "H-J") "move-window down")
(define-key *top-map* (kbd "H-H") "move-window left")
(define-key *top-map* (kbd "H-L") "move-window right")

(define-key *top-map* (kbd "H-Up") "move-focus up")
(define-key *top-map* (kbd "H-Down") "move-focus down")
(define-key *top-map* (kbd "H-Left") "move-focus left")
(define-key *top-map* (kbd "H-Right") "move-focus right")

(define-key *top-map* (kbd "H-S-Up") "move-window up")
(define-key *top-map* (kbd "H-S-Down") "move-window down")
(define-key *top-map* (kbd "H-S-Left") "move-window left")
(define-key *top-map* (kbd "H-S-Right") "move-window right")

(define-key *top-map* (kbd "H-r") "exec")
(define-key *top-map* (kbd "H-s") "exec rofi -show combi -combi-modi \"window,drun,run\" -font \"hack 20\"")
(define-key *top-map* (kbd "H-;") "colon")
(define-key *top-map* (kbd "H-:") "eval")
(define-key *top-map* (kbd "H-.") "curframe")
(define-key *top-map* (kbd "H-p") "global-pull-windowlist")

(define-key *top-map* (kbd "Print") "screenshot-area")
(define-key *top-map* (kbd "H-Print") "screenshot-window")
(define-key *top-map* (kbd "H-Sys_Req") "screenshot")

(define-key *top-map* (kbd "H-w") "chrome")
(define-key *top-map* (kbd "H-t") "tmux")
(define-key *top-map* (kbd "H-e") "emacs")
(define-key *top-map* (kbd "H-f") "nautilus")
(define-key *top-map* (kbd "H-v") "pavucontrol")
(define-key *top-map* (kbd "H-b") "blueman-manager")
(define-key *top-map* (kbd "H-RET") "exec alacritty")
(define-key *top-map* (kbd "H-/") "search-lookup")

(define-key *top-map* (kbd "H-a") "global-windowlist")
(define-key *top-map* (kbd "H-m") "mode-line")
(define-key *top-map* (kbd "H-n") "lastmsg")
(define-key *top-map* (kbd "H-N") "copy-last-message")
(define-key *top-map* (kbd "H-Q") "remove-split")
(define-key *top-map* (kbd "H-Z") "exec systemctl suspend")
(define-key *top-map* (kbd "H-u") "vsplit")
(define-key *top-map* (kbd "H-i") "hsplit")
(define-key *top-map* (kbd "H-o") "fnext")
(define-key *top-map* (kbd "H-O") "fprev")

(define-key *top-map* (kbd "H-SPC") "float-this")
(define-key *top-map* (kbd "H-S-SPC") "unfloat-this")

(define-key *top-map* (kbd "H-TAB") "next")
(define-key *top-map* (kbd "H-ISO_Left_Tab") "prev")

(define-key *top-map* (kbd "M-F4") "delete")
(define-key *top-map* (kbd "H-F10") "only")
(define-key *top-map* (kbd "H-F11") "fullscreen")

(define-key *top-map* (kbd "H-F1") "pamixer-toggle-mute")
(define-key *top-map* (kbd "H-F2") "pamixer-volume-down")
(define-key *top-map* (kbd "H-F3") "pamixer-volume-up")
(define-key *top-map* (kbd "H-S-F1") "pamixer-source-toggle-mute")
(define-key *top-map* (kbd "H-S-F2") "pamixer-source-volume-down")
(define-key *top-map* (kbd "H-S-F3") "pamixer-source-volume-up")

(setf *shell-program* "/usr/bin/bash")

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

(setf *mode-line-timeout* 2
      *time-modeline-string* "%a %b %e %k:%M"
      *screen-mode-line-format* '("[%n] %W ^> %l %P [%B] [%d]"))

(setf pamixer:*step* 2
      pamixer:*allow-boost* t
      pamixer:*source-allow-boost* t)

(setf *input-window-gravity* :top
      *message-window-gravity* :top
      *message-window-input-gravity* :top-right
      *window-border-style* :none
      *message-window-padding* 5)

(setf *input-completion-style* (make-input-completion-style-unambiguous))

(setf *mouse-focus-policy* :click)

(set-bg-color "#3F3F3F")
(set-fg-color "#DCDCCC")
(set-focus-color "#9FC59F")
(set-unfocus-color "#5F7F5F")

(setf swm-gaps:*head-gaps-size* 4
      swm-gaps:*inner-gaps-size* 4
      swm-gaps:*outer-gaps-size* 4)

(swm-gaps:toggle-gaps-on)

(setf notify:*notify-server-title-color* "^3"
      notify:*notify-server-body-color* "^7")

(notify:notify-server-on)

(run-commands
 "grename 1"
 "gnewbg 2"
 "gnewbg 3"
 "gnewbg 4")
