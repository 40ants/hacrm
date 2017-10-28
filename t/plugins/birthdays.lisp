(defpackage #:hacrm.t.plugins.birthdays
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.t.utils
        #:hacrm.plugins.birthdays)
  (:import-from #:weblocks.t.utils
                #:with-session
                #:catch-hooks
                #:assert-hooks-called))
(in-package hacrm.t.plugins.birthdays)


(plan 2)

(subtest "It should be impossible to create a fact with wrong date."
  (with-empty-db
    (with-session
      (catch-hooks (:fact-created)
        (let ((contact (hacrm.models.contact:make-contact "Vasily"))
              birthday)
          (subtest "Checking if it is possible to set date in incorrect format"
            (is-error (set-birthday contact "2017-20-14")
                      'invalid-date-format))

          (setf birthday
                (set-birthday contact "2017-08-17"))
          
          (subtest "If date is correct, returned object should contain original string in `date' slot."
            (is (date birthday)
                "2017-08-17"))

          (subtest "Hook should be called"
            (assert-hooks-called
             (contains :fact-created contact birthday))))))))


(subtest "Birthday can be set and updated"
  (with-empty-db
    (with-session
      (catch-hooks (:fact-created :fact-updated)
        (let ((contact (hacrm.models.contact:make-contact "Vasily"))
              birthday)
     
          (subtest "By default there is no birthday for a contact"
            (is (get-birthday contact)
                nil))

          ;; Set birthday for the first time.
          (setf birthday
                (set-birthday contact "1980-01-02"))

          (subtest "Now we set a birthday"
            (is (date (get-birthday contact))
                "1980-01-02"))

          (subtest "And updated a birthday"
            (set-birthday contact "1984-10-05")

            ;; And check if it was updated
            (is (date (get-birthday contact))
                "1984-10-05"))

          (subtest "When birthday is updated, a :fact-updated should be called"
            (assert-hooks-called
             (contains :fact-created contact birthday)
             (contains :fact-updated contact birthday)))

          (subtest "If date was entered with spaces, they are stripped"
            (set-birthday contact "  2001-01-02")
            (is (date (get-birthday contact))
                "2001-01-02")))))))

(finalize)
