(defpackage #:hacrm-test/plugins/birthdays
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hacrm-test/utils
        ;; #:hacrm/plugins/birthdays
        )
  ;; (:import-from #:hacrm/hooks)
  (:import-from #:weblocks-test/utils
                #:with-session
                #:catch-hooks
                #:assert-hooks-called)
  (:import-from #:hacrm/plugins/birthdays/models
                #:date
                #:invalid-date-format
                #:get-birthday
                #:set-birthday))
(in-package hacrm-test/plugins/birthdays)


(deftest raise-error-if-wrong-date
  (testing "It should be impossible to create a fact with wrong date."
    (with-empty-db
      (with-session
        (catch-hooks (:fact-created)
          (let ((contact (hacrm/models/contact:make-contact "Vasily"))
                birthday)
            (testing "Checking if it is possible to set date in incorrect format"
              (ok (rove:signals (set-birthday contact "2017-20-14")
                                'invalid-date-format)))
            
            (setf birthday
                  (set-birthday contact "2017-08-17"))
            
            (testing "If date is correct, returned object should contain original string in `date' slot."
              (ok (string= (date birthday)
                           "2017-08-17")))
            
            (testing "Hook should be called"
              (assert-hooks-called
               (:fact-created contact birthday)))))))))


(deftest birthday-updating
  (testing "Birthday can be set and updated"
    (with-empty-db
      (with-session
        (catch-hooks (:fact-created :fact-updated)
          (let ((contact (hacrm/models/contact:make-contact "Vasily"))
                birthday)
            
            (testing "By default there is no birthday for a contact"
              (ng (get-birthday contact)))

            ;; Set birthday for the first time.
            (setf birthday
                  (set-birthday contact "1980-01-02"))

            (testing "Now we set a birthday"
              (ok (equal (date (get-birthday contact))
                         "1980-01-02")))

            (testing "And updated a birthday"
              (set-birthday contact "1984-10-05")

              ;; And check if it was updated
              (ok (equal (date (get-birthday contact))
                         "1984-10-05")))

            (testing "When birthday is updated, a :fact-updated should be called"
              (assert-hooks-called
               (:fact-created contact birthday)
               (:fact-updated contact birthday)))

            (testing "If date was entered with spaces, they are stripped"
              (set-birthday contact "  2001-01-02")
              (ok (equal (date (get-birthday contact))
                         "2001-01-02")))))))))
