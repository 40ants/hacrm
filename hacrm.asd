(defsystem hacrm
  :name "hacrm"
  :version "0.0.1"
  :author "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :maintainer "Artemenko Alexander <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :description "Personal Hacker's CRM. Extendable with Common Lisp."
  :class :package-inferred-system
  :pathname "src"
  :depends-on (
               ;; :lass
               ;; :cl-strings
               ;; :montezuma
               ;; :local-time
               ;; :split-sequence
               ;; :cl-markdown
               (:version :asdf "3.3.1")
               (:version :weblocks "0.26.0")
               "hacrm/hooks"
               "hacrm/init-session"
               "hacrm/toplevel-commands"
               "hacrm/search"
               :hacrm-birthdays
               :hacrm-tags
               :hacrm-email
               :hacrm-phone
               :hacrm-notes
               ;; "hacrm/plugins/notes/plugin"
               ;; :weblocks-lass
               ;; :weblocks-stores
               ;; :weblocks-prevalence
               ;; :weblocks-ui
               ;; :cl-prevalence
               ;; :ceramic
               ;; :find-port
               ;; :3bmd
               ;; :weblocks-websocket
               ;; :mel-base   ;; from email plugin
               ;; :ubiquitous ;; from email plugin
               ;; :sanitize   ;; from email plugin
               ;; :cl-rfc2047 ;; from email plugin
               ;; :local-time
               ;; :cl-date-time-parser ;; to parse datetime in email plugin
               )
  ;; :serial t
  ;; :components ((:file hacrm)
  ;;              (:module conf
  ;;               :components ((:file stores))
  ;;               :depends-on (hacrm))
  ;;              (:module src
  ;;               :components ((:file auto-links)
  ;;                            (:file utils)
  ;;                            (:file hooks)
  ;;                            (:file commands)
  ;;                            (:file conditions)
  ;;                            (:module models
  ;;                             :components ((:file core)
  ;;                                          (:file contact)
  ;;                                          ;; (:file note)
  ;;                                          ;; (:file relation)
  ;;                                          (:file playground)
  ;;                                          (:module facts
  ;;                                           :components ((:file core)
  ;;                                                        (:file twitter)
  ;;                                                        (:file tag)))
  ;;                                          (:file feed))
  ;;                             :depends-on (utils))
  ;;                            (:module widgets
  ;;                             :components ((:file base)
  ;;                                          (:file facts)
  ;;                                          (:file notes)
  ;;                                          (:file feed)
  ;;                                          (:file contact-details)
  ;;                                          (:file contacts-list)
  ;;                                          (:file help)
  ;;                                          (:file input-box)
  ;;                                          (:file main)))
  ;;                            (:file search)
  ;;                            (:file init-session
  ;;                             :depends-on (models
  ;;                                          widgets))
  ;;                            (:file toplevel-commands)
  ;;                            (:module plugins
  ;;                             :components ((:module tags
  ;;                                           :components ((:file plugin)
  ;;                                                        (:file models)
  ;;                                                        (:file commands)
  ;;                                                        (:file widgets)
  ;;                                                        (:file search)
  ;;                                                        (:file migrations)))
  ;;                                          (:module notes
  ;;                                           :components ((:file plugin)
  ;;                                                        (:file models)
  ;;                                                        (:file widget)
  ;;                                                        (:file commands)))

  ;;                                          (:module birthdays
  ;;                                           :components ((:file plugin)
  ;;                                                        (:file models)
  ;;                                                        (:file widgets)
  ;;                                                        (:file commands)))
  ;;                                          (:module email
  ;;                                           :components ((:file plugin)
  ;;                                                        (:file models)
  ;;                                                        (:file widgets)
  ;;                                                        (:file commands)
  ;;                                                        (:file multiparts)
  ;;                                                        (:file imap)))
  ;;                                          (:module phone
  ;;                                           :components ((:file plugin)
  ;;                                                        (:file models)
  ;;                                                        (:file widgets)
  ;;                                                        (:file commands)))))

  ;;                            (:file desktop))
  ;;               :depends-on (hacrm
  ;;                            conf)))
  :in-order-to ((test-op (test-op hacrm-test))))


(register-system-packages "3bmd" '(#:3bmd-ext))
(register-system-packages "log4cl" '(#:log))
