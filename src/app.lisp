(defpackage #:hacrm/app
  (:use #:cl
        ;; #:f-underscore
        ;; #:anaphora
        )
  ;; (:import-from :hunchentoot #:header-in
  ;;       	#:set-cookie #:set-cookie* #:cookie-in
  ;;       	#:user-agent #:referer)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/server)
  (:import-from #:weblocks/app)
  (:import-from #:hacrm/db
                #:open-store
                #:close-store)
  
  (:import-from #:cl-prevalence
                #:snapshot)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks/dependencies
                #:*cache-remote-dependencies-in*
                #:get-dependencies)
  (:documentation
   "A Hackers's CRM, Weblocks application.")
  (:export
   #:restart-hacrm
   #:start-hacrm
   #:stop-hacrm
   #:backup
   #:base))
(in-package hacrm/app)

(declaim (optimize (debug 3) (safety 3) (speed 1)))


;; A macro that generates a class or this webapp

(defapp hacrm
  :prefix "/"
  :description "HaCRM: A new application"
  ;; :init-user-session 'hacrm::init-user-session
  ;; :subclasses (weblocks-ui/core:webapp) ;; отсюда брались зависимости
;;  :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
  ;; :autostart nil                     ;; have to start the app manually
  ;; :ignore-default-dependencies nil   ;; accept the defaults
  ;; :debug t
  ;; :js-backend :jquery
  )


(defmethod get-dependencies ((app hacrm))
  (append (list (weblocks-lass:make-dependency
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


(defun start-hacrm (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."

  (setf *cache-remote-dependencies-in*
        #P"/tmp/weblocks-cache/hacrm/")
  (apply #'weblocks/server:start args)

  (setf hacrm-app
        (weblocks/app:start 'hacrm))
  
  (open-store)
  (list hacrm-app))


(defun stop-hacrm ()
  "Stops the application by calling 'stop-weblocks'."
  ;; (ignore-errors
  ;;  (stop-webapp 'weblocks-cms:weblocks-cms))
  ;; (ignore-errors
  ;;  (stop-webapp 'hacrm))

  (weblocks/server:stop)

  ;; TODO: make this happen using hooks
  ;; (weblocks-stores:close-stores)
  (close-store)
  ;; (when *store*
  ;;   (snapshot *store*)
  ;;   (setf *store* nil))
  )


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
