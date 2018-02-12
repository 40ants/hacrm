(defpackage #:hacrm-test/plugins/notes
  (:use #:cl
        #:rove
        #:hamcrest/rove
        ;; #:hacrm/plugins/notes
        )
  (:import-from #:hacrm/plugins/notes/models
                #:remove-note
                #:get-notes
                #:add-note)
  (:import-from #:hacrm-test/utils
                #:with-empty-db)
  (:import-from #:hacrm/models/contact
                #:make-contact))
(in-package hacrm-test/plugins/notes)


(deftest associating-note-with-a-contact
    (with-empty-db
        (testing
         "Note can be associated with a contact"
         
         (let* ((contact1 (make-contact "Vasya"))
                (contact2 (make-contact "Masha"))
                (note1 (add-note contact1 "Foo bar")))

           (add-note contact2 "Blah minor")

           (assert-that (get-notes contact1)
                        (contains (has-slots 'hacrm/plugins/notes/models::text "Foo bar"
                                             'hacrm/plugins/notes/models::object contact1)))

           (assert-that (get-notes contact2)
                        (contains (has-slots 'hacrm/plugins/notes/models::text "Blah minor"
                                             'hacrm/plugins/notes/models::object contact2)))

           (remove-note note1)

           ;; Now we have no notes for first contact
           (assert-that (get-notes contact1)
                        (has-length 0))

           ;; But still have a note for the second contact
           (assert-that (get-notes contact2)
                        (has-length 1))))))
