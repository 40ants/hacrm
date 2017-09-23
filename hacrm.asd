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
               :3bmd
               ;; :weblocks-cms
               :weblocks-websocket
               )
  :serial t
  :components ((:file hacrm)
               (:module conf
                :components ((:file stores))
                :depends-on (hacrm))
               (:module src
                :components ((:file auto-links)
                             (:file utils)
                             (:file commands)
                             (:file conditions)
                             (:module models
                              :components ((:file contact)
                                           (:file note)
                                           (:file relation)
                                           (:file playground)
                                           (:module facts
                                            :components ((:file core)
                                                         (:file twitter)
                                                         (:file tag)))
                                           (:file feed))
                              :depends-on (utils))
                             (:module widgets
                              :components ((:file base)
                                           (:file facts)
                                           (:file notes)
                                           (:file feed)
                                           (:file contact-details)
                                           (:file contacts-list)
                                           (:file help)
                                           (:file input-box)
                                           (:file main)))
                             (:file search)
                             (:file init-session
                              :depends-on (models
                                           widgets))
                             (:file toplevel-commands)
                             (:module plugins
                              :components ((:module tags
                                            :components ((:file plugin)
                                                         (:file fact)
                                                         (:file commands)
                                                         (:file widgets)
                                                         (:file search)
                                                         (:file migrations)))
                                           (:module notes
                                            :components ((:file plugin)
                                                         (:file models)
                                                         (:file widget)
                                                         (:file commands)))

                                           (:module birthdays
                                            :components ((:file plugin)
                                                         (:file facts)
                                                         (:file widgets)
                                                         (:file commands)))
                                           (:module email
                                            :components ((:file plugin)
                                                         (:file facts)
                                                         (:file widgets)
                                                         (:file commands)))
                                           (:module phone
                                            :components ((:file plugin)
                                                         (:file facts)
                                                         (:file widgets)
                                                         (:file commands)))))

                             (:file desktop))
                :depends-on (hacrm
                             conf))))

