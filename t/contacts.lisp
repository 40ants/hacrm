(defpackage #:hacrm.t.contacts
  (:use #:cl
        #:prove
        #:hamcrest.prove))
(in-package hacrm.t.contacts)


(plan 1)

(subtest "get-name-synonyms"
  (let* ((contact (hacrm.models.contact:make-contact "саша шульгин"))
         (result (hacrm.models.contact:get-name-synonyms contact)))
    (assert-that result
                 (contains "саша шульгин"
                           "александр шульгин"
                           "шурик шульгин"
                           "шура шульгин"))))


(prove:finalize)
