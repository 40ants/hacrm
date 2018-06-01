(defpackage #:hacrm/debug
  (:use #:cl)
  (:export
   #:stop-slynk
   #:start-slynk))
(in-package hacrm/debug)


(defvar *slynk-port* nil)


(defun start-slynk ()
  (unless (and *slynk-port*
               (ignore-errors
                (slynk:connection-info)))
    (setf *slynk-port*
          (find-port:find-port :min 4005))

    (log:info "Starting Slynk server on port"
              *slynk-port*)

    (slynk:create-server :dont-close t
                         :port *slynk-port*)))

(defun stop-slynk ()
  (when *slynk-port*
    (slynk:stop-server *slynk-port*)
    (setf *slynk-port* nil)))
