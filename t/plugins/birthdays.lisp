(defpackage #:hacrm.t.plugins.birthdays
  (:use #:cl
        #:prove
        #:hamcrest.prove
        #:hacrm.t.utils
        #:hacrm.plugins.birthdays))
(in-package hacrm.t.plugins.birthdays)


(with-empty-db
  (subtest "It should be impossible to create a fact with wrong date."
    (is-error (make-birthday-fact nil "2017-20-14")
              'invalid-date-format)

    (is (date (make-birthday-fact nil "2017-08-17"))
        "2017-08-17"
        "If date is correct, returned object should contain original string in `date' slot.")))


(with-empty-db
  (subtest "Birthday can be set and updated"
    (let ((contact (hacrm.models.contact:make-contact "Vasily")))
      
      (is (get-birthday contact)
          nil)

      ;; Set birthday for the first time.
      (set-birthday contact "1980-01-02")

      (is (date (get-birthday contact))
          "1980-01-02")

      ;; Now we'll update his birthday.
      (set-birthday contact "1984-10-05")

      ;; And check if it was updated
      (is (date (get-birthday contact))
          "1984-10-05")

      ;; If date was entered with spaces, they are stripped.
      (set-birthday contact "  2001-01-02")
      (is (date (get-birthday contact))
          "2001-01-02"))))
