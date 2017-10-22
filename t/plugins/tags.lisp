(defpackage #:hacrm.t.plugins.tags
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.t.utils
        #:hacrm.plugins.tags))
(in-package hacrm.t.plugins.tags)

(plan 1)

(subtest "Tagging contact"
  (with-empty-db
    (let* ((contact (hacrm.models.contact:make-contact "Pupkin"))
           (transaction-count hacrm::*transaction-log-length*))
      ;; Created fact should have a name and a link to the contact
      (assert-that (tag-contact contact "foo-bar")
                   (has-slots 'hacrm.plugins.tags:name "foo-bar"
                              'hacrm.plugins.tags:contact contact))
      (is hacrm::*transaction-log-length*
          (+ transaction-count 2)
          "Tag creation should append two transactions. One to get uniq id and second – to create a tag."))))

(finalize)
