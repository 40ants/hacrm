;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:hacrm-asd
  (:use
   :cl
   :asdf
   :cl-who))

(in-package :hacrm-asd)

(defsystem hacrm
  :name "hacrm"
  :version "0.0.1"
  :maintainer ""
  :author ""
  :licence ""
  :description "hacrm"
  :depends-on (:weblocks
               :weblocks-twitter-bootstrap-application
               :weblocks-cms)
  :components ((:file "hacrm")
               (:module conf
                :components ((:file "stores"))
                :depends-on ("hacrm"))
               (:module src
                :components ((:file "init-session"))
                :depends-on ("hacrm" conf))))

