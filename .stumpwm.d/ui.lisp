(in-package :stumpwm)

(set-bg-color "#3F3F3F")
(set-fg-color "#DCDCCC")
(set-focus-color "#9FC59F")
(set-unfocus-color "#5F7F5F")

(let ((font-cache (merge-pathnames ".fonts/font-cache.sexp"
                                   (user-homedir-pathname))))
  (when (not (probe-file font-cache))
    (clx-truetype::cache-fonts)))

(xft-fix:start-loop)

(let* ((scale (parse-integer (getenv "JACK_SCALE")))
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
