(defpackage #:hacrm-test/utils-tests
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hacrm-test/utils)
  (:import-from #:hacrm/models/contact
                #:get-all-contacts)
  (:export
   #:with-empty-db))
(in-package hacrm-test/utils-tests)


(deftest test-if-separate-database-is-created
  (testing "Checking if a separate database for unittests is created"
           (with-empty-db
             (testing "Macro with-empty-db should reset database"
               (assert-that (get-all-contacts)
                            (has-length 0)))
             (ok (equal (hacrm/models/core::get-next-id)
                        1)
                 "And object ids should start from 1"))))
