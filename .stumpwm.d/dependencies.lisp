(ql:quickload "slynk")

(defcommand slynk () ()
            (slynk:create-server :port 4004
                                 :dont-close t))
