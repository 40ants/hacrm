(defsystem hacrm-test
  :class :package-inferred-system
  :pathname "t"
  :depends-on (:hacrm
               ;; :hamcrest/rove
               ;; :weblocks-testutils
               "hacrm-test/utils-tests"
               "hacrm-test/facts"
               "hacrm-test/contacts"
               "hacrm-test/plugins/birthdays"
               "hacrm-test/plugins/tags"
               "hacrm-test/plugins/notes"
               "hacrm-test/plugins/phones"
               "hacrm-test/plugins/email")
  ;; :components ((:module t
  ;;               :components ((:file utils)
  ;;                            (:file utils-tests)
  ;;                            (:file facts)
  ;;                            (:file contacts)
  ;;                            (:module plugins
  ;;                             :components ((:file birthdays)
  ;;                                          (:file tags)
  ;;                                          (:file notes)
  ;;                                          (:file phones)
  ;;                                          (:file email))))))
  :perform (test-op :after (op c)
                    (symbol-call :rove :run c)))
