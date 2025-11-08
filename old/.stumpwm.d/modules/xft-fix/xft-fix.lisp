;;;; xft-fix.lisp

(in-package #:xft-fix)

(defvar *clear-font-repeat* 10 "Clear font every *clear-font-minutes*")
(defvar *clear-font-delay* 0 "Clear font every *clear-font-minutes* with a *clear-font-delay* minutes")
(defvar *clear-font-cache-timer* nil)

(defun clear-font-cache ()
  (loop for font in (stumpwm::screen-fonts (stumpwm:current-screen))
	when (typep font 'xft:font)
	  do (clrhash (xft::font-string-line-bboxes font))
	     (clrhash (xft::font-string-line-alpha-maps font))
	     (clrhash (xft::font-string-bboxes font))
	     (clrhash (xft::font-string-alpha-maps font))))

(defun font-timer-in-timer-list? (&optional (font-timer *clear-font-cache-timer*)) 
  (if font-timer
      (some (lambda (x) (eq x font-timer)) stumpwm::*timer-list*)))

(defun make-font-cache-clear-time (&optional (delay-mins *clear-font-delay*)
				   (repeat-mins *clear-font-repeat*))
  "Make a new timer, if timer already exists then replace it with new values."
  (if (font-timer-in-timer-list?)
      (cancel-timer *clear-font-cache-timer*))
; this is a useless check since I am deleting the timer regardless
;  (if
;   (equal (* repeat-mins 60) (timer-repeat *clear-font-cache-timer*))
   (setq *clear-font-cache-timer* (run-with-timer 
				   (* 60 delay-mins)
				   (* 60 repeat-mins)
				   #'clear-font-cache)))

(defun start-loop ()
  (make-font-cache-clear-time))
