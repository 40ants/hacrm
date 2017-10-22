(defpackage #:hacrm.t.contacts
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:import-from #:hacrm.models.core
                #:get-object-id)
  (:import-from #:hacrm.models.contact
                #:make-contact
                #:get-name-synonyms
                #:name
                #:all-contacts)
  (:import-from #:hacrm.t.utils
                #:with-empty-db))
(in-package hacrm.t.contacts)


(plan 2)

(subtest "get-name-synonyms"
  (with-empty-db
    (let* ((contact (make-contact "саша шульгин"))
           (result (get-name-synonyms contact)))
      (assert-that result
                   (contains "саша шульгин"
                             "александр шульгин"
                             "шурик шульгин"
                             "шура шульгин")))))

(subtest "make-contact saves data to database and assigns it a sequential id"
  (with-empty-db
    (let* ((contact (make-contact "саша шульгин"))
           (result (all-contacts)))
      (subtest "make-contact should return created object"
        (assert-that contact
                     (has-slots 'name "саша шульгин"))
        (is (get-object-id contact)
            1
            "Contact has id=1"))
    
      (subtest "Database should contain one contact with given name."
        (assert-that result
                     (contains (has-slots 'name "саша шульгин"))))

      (subtest "Next contact should have incremented id"
        (let ((contact (make-contact "next contact")))
          (is (get-object-id contact)
              2
              "Next contact's id is 2"))))))

(prove:finalize)
