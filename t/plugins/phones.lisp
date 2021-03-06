(defpackage #:hacrm-test/plugins/phone
  (:use #:cl
        #:rove
        #:hamcrest/rove
        #:hacrm-test/utils
        ;; #:hacrm/plugins/phone
        ))
(in-package hacrm-test/plugins/phone)

(deftest adding-phone-to-the-contact
    (with-empty-db
        (testing
         "Adding phone to a contact"
         (let ((contact (hacrm/models/contact:make-contact "Vasya")))
           (add-phone contact "+7 916 015 7719")
           (add-phone contact "8 926 001 2456")
           (assert-that (get-phones contact)
                        (contains-in-any-order
                         (has-slots 'number "+7 916 015 7719")
                         (has-slots 'number "8 926 001 2456")))))))


(deftest removing-phoen
    (with-empty-db
        (testign
         "Removing phone"
         (let ((contact (hacrm/models/contact:make-contact "Vasya")))
           (add-phone contact "+7 916 015 7719")
           (add-phone contact "8 926 001 2456")
           ;; Now we'll remove the first phone
           (remove-phone contact "+7 916 015 7719")

           ;; And expect it to be removed from the database
           (assert-that (get-phones contact)
                        (contains
                         (has-slots 'number "8 926 001 2456")))))))

