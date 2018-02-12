(defpackage #:hacrm-test/plugins/tags
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hacrm-test/utils
        ;; #:hacrm/plugins/tags
        )
  (:import-from #:hacrm/plugins/tags/models
                #:untag-contact
                #:tag-contact
                #:get-contact-tags)
  (:import-from #:weblocks-test/utils
                #:with-session
                #:catch-hooks
                #:assert-hooks-called)
  (:import-from #:hacrm/db
                #:*transaction-log-length*))
(in-package hacrm-test/plugins/tags)


(deftest tagging
    (testing
     "Tagging contact"
     (with-empty-db
         (with-session
             (catch-hooks (:fact-created)
                          (let* ((contact1 (hacrm/models/contact:make-contact "Sasha"))
                                 (contact2 (hacrm/models/contact:make-contact "Mashax"))
                                 (transaction-count *transaction-log-length*))

                            ;; Created fact should have a name and a link to the contact
                            (let ((tag (hacrm/plugins/tags/models::tag-contact contact1 "foo-bar")))
                              (testing
                               "Result of tag-contact call should be a tag object"
                               (assert-that tag
                                            (has-slots 'hacrm/plugins/tags/models::name "foo-bar"
                                                       'hacrm/models/facts/core::contact contact1)))
                              (testing
                               "Tag creation should append two transactions. One to get uniq id and second – to create a tag."
                                (ok (equal *transaction-log-length*
                                           (+ transaction-count 2))))

                              (testing
                               "Contact 1 should have a tag \"foo-bar\" now"
                               (assert-that (get-contact-tags contact1)
                                            (contains (has-slots
                                                       'hacrm/plugins/tags/models::name "foo-bar"))))

                              (testing
                               "But second contact wasn't tagged"
                                (ok (null (get-contact-tags contact2))))

                              (testing
                               "And one call to fact-created hook should be made"
                               (assert-hooks-called
                                (:fact-created contact1 tag))))))))))


(deftest test-untagging
    (testing
     "Untagging contact"
     (with-empty-db
         (with-session
             (catch-hooks (:fact-removed)
                          (let* ((contact1 (hacrm/models/contact:make-contact "Sasha"))
                                 (contact2 (hacrm/models/contact:make-contact "Mashax"))
                                 tag)

                            ;; It is better to use same tag for both contacts
                            ;; to ensure that tag does not removed by name from
                            ;; all contacts instead from one given to the untag-contact
                            (setf tag
                                  (tag-contact contact1 "foo"))
                            (tag-contact contact1 "bar")
                                 
                            (tag-contact contact2 "foo")

                            ;; Now we need to untag the first contact
                            ;; and ensure that tag on the second contact
                            ;; still in the database
                                 
                            ;; Let see what happend inside untag contact
                            ;; I'm hittint M-. on the function's name
                            (untag-contact contact1 "foo")

                            (testing
                             "Checking tags on our first contact, they should contain only bar, because foo tag was removed"
                             (assert-that (get-contact-tags contact1)
                                          (contains (has-slots
                                                     'hacrm/plugins/tags/models::name
                                                     "bar"))))

                            (testing
                             "But second contact should have one tag"
                             (assert-that (get-contact-tags contact2)
                                          (contains (has-slots
                                                     'hacrm/plugins/tags/models::name
                                                     "foo"))))

                            (testing
                             "Untagging should emit a hook call"
                             (assert-hooks-called
                              (:fact-removed contact1 tag)))))))))
