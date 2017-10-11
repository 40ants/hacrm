(defpackage #:hacrm.t.contacts
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:import-from #:hacrm.models.contact
                #:make-contact
                #:get-name-synonyms
                #:name
                #:all-contacts)
  (:import-from #:hacrm.t.utils
                #:with-empty-db))
(in-package hacrm.t.contacts)


(plan 3)

(with-empty-db
 (subtest "get-name-synonyms"
   (let* ((contact (make-contact "саша шульгин"))
          (result (get-name-synonyms contact)))
     (assert-that result
                  (contains "саша шульгин"
                            "александр шульгин"
                            "шурик шульгин"
                            "шура шульгин")))))

(with-empty-db
 (subtest "make-contact saves data to database"
   (let* ((contact (make-contact "саша шульгин"))
          (result (all-contacts)))
     (subtest "make-contact should return created object"
       (assert-that contact
                    (has-slots 'name "саша шульгин")))
     (subtest "Database should contain one contact with given name."
       (assert-that result
                    (contains (has-slots 'name "саша шульгин")))))))


(prove:finalize)
