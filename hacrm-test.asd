(defpackage #:hacrm-test-asd
  (:use
   :cl
   :asdf))

(in-package hacrm-test-asd)


(defsystem hacrm-test
  :depends-on (:hacrm
               :hamcrest-prove
               :weblocks-testutils)
  :serial t
  :components ((:module t
                :components ((:test-file utils)
                             (:test-file facts)
                             (:test-file contacts)
                             (:module plugins
                              :components ((:test-file birthdays)
                                           (:test-file tags)
                                           (:test-file notes)
                                           (:test-file phones)
                                           (:test-file email)))))))
