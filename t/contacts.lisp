(defpackage #:hacrm-test/contacts
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  (:import-from #:hacrm/models/core
                #:get-object-id)
  (:import-from #:hacrm/models/contact
                #:make-contact
                #:get-name-synonyms
                #:name
                #:get-all-contacts)
  (:import-from #:hacrm-test/utils
                #:with-empty-db))
(in-package hacrm-test/contacts)


(deftest get-name-synonyms-test
  (with-empty-db
    (let* ((contact (make-contact "саша шульгин"))
           (result (get-name-synonyms contact)))
      (assert-that result
                   (contains "саша шульгин"
                             "александр шульгин"
                             "шурик шульгин"
                             "шура шульгин")))))

(deftest saving-data-to-the-database
  (testing "make-contact saves data to database and assigns it a sequential id"
    (with-empty-db
      (let* ((contact (make-contact "саша шульгин"))
             (result (get-all-contacts)))
        (testing "make-contact should return created object"
          (assert-that contact
                       (has-slots 'name "саша шульгин"))
          (ok (eql (get-object-id contact)
                   1)
              "Contact has id=1"))
        
        (testing "Database should contain one contact with given name."
          (assert-that result
                       (contains (has-slots 'name "саша шульгин"))))

        (testing "Next contact should have incremented id"
          (let ((contact (make-contact "next contact")))
            (ok (eql (get-object-id contact)
                     2)
                "Next contact's id is 2")))))))
