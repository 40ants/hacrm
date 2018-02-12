(defpackage #:hacrm/desktop
  (:use #:cl)
  (:export
   #:start-dev-app
   #:stop-dev-app))
(in-package hacrm/desktop)

(defvar *window* nil)


(defun run ()
  (let ((port (find-port:find-port)))
    (log:info "Starting HACRM on port"
              port)
    (hacrm:start-hacrm :port port)

    (log:debug "Creating window")
    (setf *window*
          (ceramic:make-window :url (format nil "http://localhost:~D/" port)))
    (ceramic:show *window*)))


(defun start-dev-app ()
  "Стартует приложение из REPL, для разработки."
  (ceramic:start)
  (run))

(defun stop-dev-app ()
  (hacrm:stop-hacrm)
  (ceramic:stop))

(ceramic:define-entry-point :hacrm ()
  ;; we need to muffle warnings from Weblocks about
  ;; missing asset files
  (handler-bind ((warning #'muffle-warning))
    (run)))
