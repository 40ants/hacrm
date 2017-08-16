(in-package asdf)

(defpackage #:hacrm.t.facts
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.t.utils
        #:hacrm.models.contact
        #:hacrm.models.facts.core)
  (:import-from #:hacrm.models.facts.core))
(in-package hacrm.t.facts)


(with-empty-db
  (subtest "If there is no facts about contact, then there is no groups."
    (let ((contact (make-contact "Petya")))
      (is (fact-groups contact)
          nil))))
