(defpackage #:hacrm.t.plugins.notes
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.plugins.notes)
  (:import-from #:hacrm.t.utils
                #:with-empty-db))
(in-package hacrm.t.plugins.notes)


(plan 1)

(with-empty-db
  (subtest "Note can be associated with a contact"
    (let* ((contact1 (hacrm.models.contact:make-contact "Vasya"))
           (contact2 (hacrm.models.contact:make-contact "Masha"))
           (note1 (add-note contact1 "Foo bar")))

      (add-note contact2 "Blah minor")

      (assert-that (get-notes contact1)
                   (contains (has-slots 'hacrm.plugins.notes::text "Foo bar"
                                        'hacrm.plugins.notes::object contact1)))

      (assert-that (get-notes contact2)
                   (contains (has-slots 'hacrm.plugins.notes::text "Blah minor"
                                        'hacrm.plugins.notes::object contact2)))

      (remove-note note1)

      ;; Now we have no notes for first contact
      (assert-that (get-notes contact1)
                   (has-length 0))

      ;; But still have a note for the second contact
      (assert-that (get-notes contact2)
                   (has-length 1)))))

(finalize)
