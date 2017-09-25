(defpackage #:hacrm-test-asd
  (:use
   :cl
   :asdf))

(in-package hacrm-test-asd)


(defsystem hacrm-test
  :depends-on (:hacrm
               :hamcrest-prove)
  :serial t
  :components ((:module t
                :components ((:file utils)
                             (:file facts)
                             (:file contacts)
                             (:module plugins
                              :components ((:file birthdays)))))))
