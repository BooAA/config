;;;; xft-fix.asd

(asdf:defsystem #:xft-fix
  :description "Describe xft-fix here"
  :author "Kamy"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "xft-fix")))
