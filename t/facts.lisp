(defpackage #:hacrm.t.facts
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.models.facts.core)
  (:import-from #:hacrm.models.facts.core))
(in-package hacrm.t.facts)


(subtest "Fact groups"
  (let ((contact (make-contact )))
    (is (fact-groups)
        (list :tags))))
