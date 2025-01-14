;; -*- lexical-binding: t; -*-

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; (push '(undecorated . t) default-frame-alist)
(push '(left-fringe . 2) default-frame-alist)
(push '(right-fringe . 2) default-frame-alist)

;; (push '(fullscreen . maximized) initial-frame-alist)

(set-face-attribute 'default t
                    :family "hack"
                    :height 80
                    :weight 'normal
                    :width 'normal)

(setopt custom-file (make-temp-file "emacs-custom-"))

(setopt gc-cons-threshold most-positive-fixnum)

(setopt read-process-output-max (* 4 1024 1024)
        process-adaptive-read-buffering nil)

(setopt load-prefer-newer t)

(setopt native-comp-async-report-warnings-errors 'silent)

(setopt use-package-enable-imenu-support t)

(setopt native-comp-speed -1)

(provide 'early-init)
