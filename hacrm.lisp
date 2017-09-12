(defpackage #:hacrm
  (:use :cl :weblocks
        :cl-who
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks.")
  (:export
   #:restart-hacrm
   #:%widget
   #:start-hacrm
   #:stop-hacrm
   #:backup
   #:base))

(in-package :hacrm)

(declaim (optimize (debug 3) (speed 1)))

;; A macro that generates a class or this webapp

(defwebapp hacrm
  :prefix "/"
  :description "hacrm: A new application"
  :init-user-session 'hacrm::init-user-session
  :subclasses (weblocks.ui.app:webapp)
;;  :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
  :autostart nil                     ;; have to start the app manually
  :ignore-default-dependencies nil   ;; accept the defaults
  :debug t
  :js-backend :jquery
  )


(defmethod weblocks.dependencies:get-dependencies ((app hacrm))
  (append (list (weblocks.lass:make-dependency
                 '(body
                   :position absolute
                   :height 100%
                   :min-height 100%
                   :width 100%
                   :margin 0
                   :padding 0

                   ;; Common "reset" rules
                   (*
                    :box-sizing "border-box"
                    :margin 0
                    :padding 0
                    :border 0
                    ;; to make element's height calculation easier, will use
                    ;; 20px as a default font-size
                    :font-size 20px
                    :line-height 30px
                    :font-family helvetica
                    )
                   (a
                    ;; special color for links
                    :color "#0071d8"))))
          (call-next-method)))


;; Top level start & stop scripts

(defvar hacrm-app nil
  "Инстанс приложения hacrm.")

(defvar admin-app nil
  "Инстанс приложения с админкой.")


(defun start-hacrm (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."

  (setf weblocks.dependencies:*cache-remote-dependencies-in*
        #P"/tmp/weblocks-cache/hacrm/")
  (apply #'weblocks.server:start-weblocks args)

  ;; For some reason admin-app should be started before
  ;; all other app, otherwise weblocks::*active-webapps*
  ;; will contain only weblocks-cms application
  ;; and static files will not be served propertly
  ;;
  ;; Hmm, but now I tried to swap them again, and
  ;; seems it works anyway.
  (setf hacrm-app
        (start-webapp 'hacrm))
  ;; (setf admin-app
  ;;       (start-webapp 'weblocks-cms:weblocks-cms))

  (weblocks-stores:open-stores)
  
  (list hacrm-app admin-app))


(defun stop-hacrm ()
  "Stops the application by calling 'stop-weblocks'."
  ;; (ignore-errors
  ;;  (stop-webapp 'weblocks-cms:weblocks-cms))
  (ignore-errors
   (stop-webapp 'hacrm))
  (weblocks.server:stop-weblocks)

  ;; TODO: make this happen using hooks
  (weblocks-stores:close-stores))


;; (defun reset-hunchentoot-routes ()
;;   (setf hunchentoot:*dispatch-table*
;;         (list
;;          'hunchentoot:dispatch-easy-handlers
;;          'weblocks::weblocks-dispatcher)

;;         weblocks-utils::*dispatchers-cache*
;;         (make-hash-table :test #'equal)))


(defun restart-hacrm (&rest args)
  (stop-hacrm)
  (sleep 3)
  (apply #'start-hacrm args))


(defun switch-to-db (path)
  (weblocks-stores:close-store *hacrm-store*)
  (setf (cl-prevalence::get-directory *hacrm-store*)
        path)
  (weblocks-stores:open-store *hacrm-store*))


(defun dev-db ()
  (switch-to-db *dev-db-path*))


(defun default-db ()
  (switch-to-db *default-db-path*))



;; Модель шифра

(defvar f1 '(1 3 2 3 3 2 4 3))

(defvar f2 '(1 4 2 2 2 4 3 3))

(defvar f3 '(4 5 4 6 5 5 6 5))

(defvar result '(8 12 10 10 12 10 8 12))


(defun rotate (l)
  (append (cdr l) (list (car l))))


(defun is-good ()
  (let ((results (loop for i1 in f1
                       for i2 in f2
                       for i3 in f3
                       for r in result
                       collect (equal (+ i1 i2 i3)
                                      r))))
    (notany #'null results)))


(defun calculate ()
  (loop for i from 1 to 8
        do (loop for i from 1 to 8
                 do (loop for i from 1 to 8
                          when (is-good)
                            do (return-from calculate :done)
                          do (setf f3 (rotate f3)))
                 do (setf f2 (rotate f2)))
        do (setf f1 (rotate f1))))
