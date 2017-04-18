
(defpackage #:hacrm
  (:use :cl :weblocks
        :cl-who
        :f-underscore :anaphora)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :hacrm)

(export '(start-hacrm stop-hacrm))

;; A macro that generates a class or this webapp

(defwebapp hacrm
  :prefix "/"
  :description "hacrm: A new application"
  :init-user-session 'hacrm::init-user-session
;;  :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
  :autostart nil                     ;; have to start the app manually
  :ignore-default-dependencies nil   ;; accept the defaults
  :debug t
  :js-backend :prototype
  )

;; Top level start & stop scripts

(defun start-hacrm (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'hacrm)
  (weblocks-cms:regenerate-model-classes)
  (start-webapp 'weblocks-cms:weblocks-cms))

(defun stop-hacrm ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'weblocks-cms:weblocks-cms)
  (stop-webapp 'hacrm)
  (stop-weblocks))

