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
    (let* ((contact1 (hacrm.models.contact:make-contact "Sasha"))
           (contact2 (hacrm.models.contact:make-contact "Mashax"))
           (transaction-count hacrm::*transaction-log-length*))
      ;; Created fact should have a name and a link to the contact
      (assert-that (tag-contact contact1 "foo-bar")
                   (has-slots 'hacrm.plugins.tags:name "foo-bar"
                              'hacrm.plugins.tags:contact contact1))
      (is hacrm::*transaction-log-length*
          (+ transaction-count 2)
          "Tag creation should append two transactions. One to get uniq id and second – to create a tag.")

      ;; First contact now should have one tag
      (assert-that (get-contact-tags contact1)
                   (contains (has-slots
                              'hacrm.plugins.tags:name "foo-bar")))

      ;; But second contact wasn't tagged
      (is (get-contact-tags contact2)
          nil))))


(subtest "Untagging contact"
  (with-empty-db
    (let* ((contact1 (hacrm.models.contact:make-contact "Sasha"))
           (contact2 (hacrm.models.contact:make-contact "Mashax")))

      ;; It is better to use same tag for both contacts
      ;; to ensure that tag does not removed by name from
      ;; all contacts instead from one given to the untag-contact
      (tag-contact contact1 "foo")
      (tag-contact contact1 "bar")
      
      (tag-contact contact2 "foo")

      ;; Now we need to untag the first contact
      ;; and ensure that tag on the second contact
      ;; still in the database
      
      ;; Let see what happend inside untag contact
      ;; I'm hittint M-. on the function's name
      (untag-contact contact1 "foo")

      (subtest "Checking tags on our first contact, they should contain only bar, because foo tag was removed"
        (assert-that (get-contact-tags contact1)
                     (contains (has-slots
                                'hacrm.plugins.tags:name
                                "bar"))))

      (subtest "But second contact should have one tag"
        (assert-that (get-contact-tags contact2)
                     (contains (has-slots
                                'hacrm.plugins.tags:name
                                "foo")))))))

(finalize)
