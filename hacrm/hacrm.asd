;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:hacrm-asd
  (:use
   :cl
   :asdf))

(in-package :hacrm-asd)

(defsystem hacrm
  :name "hacrm"
  :version "0.0.1"
  :maintainer ""
  :author ""
  :licence ""
  :description "hacrm"
  :depends-on (:lass
               :local-time
               :split-sequence
               :cl-markdown
               :weblocks
               :weblocks-lass
               :weblocks-stores
               :weblocks-prevalence
               :weblocks-ui
               :cl-prevalence
               ;;:weblocks-twitter-bootstrap-application
               :ceramic
               :swank
               :find-port
               ;; :weblocks-cms
               )
  :serial t
  :components ((:file hacrm)
               (:module conf
                :components ((:file stores))
                :depends-on (hacrm))
               (:module src
                :components ((:file utils)
                             (:file query)
                             (:module models
                              :components ((:file contact)
                                           (:file note)
                                           (:file playground)
                                           (:module facts
                                            :components ((:file core)
                                                         (:file twitter)
                                                         (:file tag))))
                              :depends-on (utils))
                             (:file search)
                             (:module widgets
                              :components ((:file notes)
                                           (:file contact-details
                                            :depends-on (notes))
                                           (:file contacts-list)))
                             (:file init-session
                              :depends-on (models
                                           widgets))

                             (:file desktop))
                :depends-on (hacrm
                             conf))))

