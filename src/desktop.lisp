(defpackage #:hacrm/desktop
  (:use #:cl #:defmain)
  (:import-from #:log4cl-json)
  (:import-from #:ceramic
                #:show
                #:make-window
                #:define-entry-point)
  (:import-from #:hacrm/app
                #:stop-hacrm
                #:start-hacrm)
  (:import-from #:hacrm/utils
                #:get-machine-name)
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
  (let* ((interface "127.0.0.1")
         (port (find-port:find-port :min 9000 :interface interface)))
    (log:info "Starting HACRM on port"
              port)
    (setf *port* port)
    (start-hacrm :port port :interface interface :debug debug)

    (log:debug "Creating window")
    (setf *window*
          (make-window :url (format nil "http://localhost:~D/" port)))

    (setf (ceramic.window:title *window*)
          (if debug
              (format nil "HACRM (~A)"
                      (get-machine-name))
              "HACRM"))
    
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


(defmain main ((debug "Start slynk, turn on verbose logging, etc." :flag t)
               (break "Break into debugger right after the start." :flag t)
               (slynk "Start slynk, but run in production mode." :flag t)
               (no-ceramic "Don't start Electron application" :flag t))
  "Starts Ceramic application.

Set 'releasep' argument to nil if you start it from the REPL.
Otherwise, Ceramic will search Electron binary in the same
directory where executable file resides."

  ;; It is important to reinitialize a state each time,
  ;; to make different uuid4s.
  (setf *random-state*
        (make-random-state t))
  
  (when break
    (sb-ext:enable-debugger)
    (break))

  (log4cl-json:setup :plain t
    :level (if debug
               :debug
               :warn))
  
  (when debug
    (weblocks/debug:on))

  ;; Always use release mode because otherwise
  ;; Ceramic will not find app in the directory,
  ;; created by ./build.sh
  (setf ceramic::*releasep* t)
  
  (when (or debug slynk)
    ;; We'll start swank only of application was started not
    ;; from the slime's repl.
    (start-slynk))

  ;; Start Ceramic and Electron
  (cond
    (no-ceramic (format t "Waiting for SLY.~%")
                (loop do (sleep 5)))
    (t 
     ;; Start Ceramic and Electron
     ;; Without setting of the interface, find-port searches a port
     ;; for websocket on 127.0.0.1, however it is opened on 0.0.0.0
     ;; and this prevents opening of the second application.
     ;; 
     ;; TODO: fix this and open app on 127.0.0.1 
     (let ((find-port:*default-interface* "0.0.0.0"))
       (ceramic:start))

     (handler-bind ((t (lambda (condition)
                         (log:info "Exception caught" condition)
                         (uiop:print-condition-backtrace condition)

                         (stop-slynk)
                         (log:info "Quitting")
                         (ceramic:quit))))
       (run :debug debug)
       (loop while (ceramic.driver:driver-running ceramic::*driver*)
             do (sleep 1))))))
