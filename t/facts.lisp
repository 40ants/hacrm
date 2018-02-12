(defpackage #:hacrm-test/facts
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hacrm-test/utils
        #:hacrm/models/facts/core)
  
  (:import-from #:hacrm/models/contact
                #:make-contact))
(in-package hacrm-test/facts)

(deftest no-groups-if-no-facts
  (with-empty-db
      (testing "If there is no facts about contact, then there is no groups."
        (let ((contact (make-contact "Petya")))
          (ng (fact-groups contact))))))

