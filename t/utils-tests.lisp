(defpackage #:hacrm.t.utils-tests
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.t.utils)
  (:export
   #:with-empty-db))
(in-package hacrm.t.utils-tests)


(plan 1)

(subtest "Checking if a separate database for unittests is created"
  (with-empty-db
    (is (length (hacrm.models.contact:all-contacts))
        0
        "Macro with-empty-db should reset database")
    (is (hacrm.models.core::get-next-id)
        1
        "And object ids should start from 1")))

(finalize)
