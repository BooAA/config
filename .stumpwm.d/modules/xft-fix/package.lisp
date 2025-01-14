;;;; package.lisp

(defpackage #:xft-fix
  (:use #:cl :xft :stumpwm)
  (:export #:start-loop #:*clear-font-repeat* #:*clear-font-delay* #:*clear-font-cache-timer*))
