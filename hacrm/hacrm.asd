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
               :cl-strings
               :montezuma
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
                             (:file commands)
                             (:file conditions)
                             (:module models
                              :components ((:file contact)
                                           (:file note)
                                           (:file playground)
                                           (:module facts
                                            :components ((:file core)
                                                         (:file twitter)
                                                         (:file tag))))
                              :depends-on (utils))
                             (:module widgets
                              :components ((:file base)
                                           (:file notes)
                                           (:file contact-details)
                                           (:file contacts-list)
                                           (:file help)
                                           (:file input-box)
                                           (:file main)))
                             (:file search)
                             (:file init-session
                              :depends-on (models
                                           widgets))
                             (:module plugins
                              :components ((:module tags
                                            :components ((:file plugin)
                                                         (:file fact)
                                                         (:file commands)
                                                         (:file render)
                                                         (:file search)
                                                         (:file migrations)))))

                             (:file desktop))
                :depends-on (hacrm
                             conf))))

