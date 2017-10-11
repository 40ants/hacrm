(defpackage #:hacrm.t.plugins.email
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:import-from #:hacrm.models.contact
                #:make-contact
                #:find-contacts-by)
  (:import-from #:hacrm.t.utils
                #:with-empty-db)
  (:import-from #:hacrm.plugins.email
                #:add-email
                #:remove-email
                #:get-emails
                #:address
                #:already-exists))
(in-package hacrm.t.plugins.email)


(plan 3)


(with-empty-db
  (subtest "Email can be associated with a contact"
    (let ((contact (make-contact "Pupkin")))
      (add-email contact "some@example.com")

      (assert-that (get-emails contact)
                   (contains (has-slots 'address
                                        "some@example.com")))

      (let ((removed (remove-email contact "some@example.com")))

        ;; Function remove-email should return removed facts
        (assert-that removed
                     (contains (has-slots 'address
                                          "some@example.com")))

        (assert-that (get-emails contact)
                     (has-length 0))))))


(with-empty-db
  (subtest "Contact can be searched by email"
    (let ((contact (make-contact "Pupkin")))
      (add-email contact "some@example.com")

      (let ((search-results (find-contacts-by :email "some@example.com")))
        (assert-that search-results
                     (contains (has-slots 'hacrm.models.contact:name
                                          "Pupkin")))))))


(with-empty-db
  (subtest "When email is added to the second contact, error should be raised"
    (let ((contact (make-contact "Pupkin"))
          (second-contact (make-contact "Chubaka")))
      (add-email contact "some@example.com")

      (is-condition (add-email second-contact "some@example.com")
                    'already-exists)/)))


(prove:finalize)
