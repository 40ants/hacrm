(defpackage #:hacrm/desktop
  (:use #:cl #:defmain)
  (:import-from #:ceramic
                #:show
                #:make-window
                #:define-entry-point)
  (:import-from #:hacrm/app
                #:stop-hacrm
                #:start-hacrm)
  (:import-from #:hacrm/debug
                #:start-slynk
                #:stop-slynk)
  (:export
   #:start-dev-app
   #:stop-dev-app))
(in-package hacrm/desktop)

(defvar *window* nil)
(defvar *port* nil)


(defun run (&key debug)
  (let ((port (find-port:find-port :min 9000)))
    (log:info "Starting HACRM on port"
              port)
    (setf *port* port)
    (start-hacrm :port port)

    (log:debug "Creating window")
    (setf *window*
          (make-window :url (format nil "http://localhost:~D/" port)))
    (show *window*)))


(defun start-dev-app ()
  "Стартует приложение из REPL, для разработки."
  (ceramic:start)
  (run))

(defun stop-dev-app ()
  (stop-hacrm)
  (ceramic:stop))

;; (define-entry-point :hacrm ()
;;   ;; we need to muffle warnings from Weblocks about
;;   ;; missing asset files
;;   (handler-bind ((warning #'muffle-warning))
;;     (log:error "STARTING THE SERVER")
;;     (run)))


(defmain main ((debug "Start slynk, turn on verbose logging, etc." :flag t))
  "Starts Ceramic application.

Set 'releasep' argument to nil if you start it from the REPL.
Otherwise, Ceramic will search Electron binary in the same
directory where executable file resides."

  (when debug
    (log:config :sane2 :debug)
    (weblocks/debug:on))
  
  (let ((ceramic::*releasep* (not debug)))
         
    (when debug
      ;; We'll start swank only of application was started not
      ;; from the slime's repl.
      (start-slynk))

    ;; Start Ceramic and Electron
    (ceramic:start)
    (handler-bind ((t (lambda (condition)
                        (log:info "Exception caught" condition)
                        (uiop:print-condition-backtrace condition)

                        (stop-slynk)
                        (log:info "Quitting")
                        (ceramic:quit))))
      (run)
      (loop while (ceramic.driver:driver-running ceramic::*driver*)
            do (sleep 1)))))
