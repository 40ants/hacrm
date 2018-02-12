(defpackage #:hacrm-test/plugins/email
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  (:import-from #:hacrm/models/contact
                #:make-contact
                #:find-contacts-by)
  (:import-from #:hacrm-test/utils
                #:with-empty-db)
  (:import-from #:weblocks-test/utils
                #:with-session
                #:catch-hooks
                #:assert-hooks-called)
  (:import-from #:hacrm/plugins/email/models
                #:add-email
                #:remove-email
                #:get-emails
                #:address
                #:already-exists)
  ;; Actually, mel-base includes packages :mel
  (:import-from #:mel-base)
  (:import-from #:hacrm/plugins/email/imap
                #:process-messages
                #:set-password))
(in-package hacrm-test/plugins/email)


(deftest associating-email-with-a-contact
  (testing
      "Email can be associated with a contact"
    (with-empty-db
      (with-session
        (catch-hooks (:fact-created :fact-removed)
          (let* ((contact (make-contact "Pupkin"))
                 (some-email (add-email contact "some@example.com"))
                 (another-email (add-email contact "another@example.com")))
            (ok (typep some-email 'hacrm/plugins/email/models::email)
                "Function add-email returns a fact object")
            
            (testing "Email creation calls :fact-created hook"
              (assert-hooks-called
               (:fact-created contact some-email)
               (:fact-created contact another-email)))

            (testing "After email was added, it can be retrived with get-emails function"
              (assert-that (get-emails contact)
                           (contains (has-slots 'address
                                                "another@example.com")
                                     (has-slots 'address
                                                "some@example.com"))))

            (let ((removed-emails (remove-email contact "some@example.com")))

              (testing "Function remove-email should return only removed facts"
                (assert-that removed-emails
                             (contains (has-slots 'address
                                                  "some@example.com"))))

              (testing "Email removal calls :fact-removed hook"
                (assert-hooks-called
                 (:fact-created contact some-email)
                 (:fact-created contact another-email)
                 (:fact-removed contact (first removed-emails))))
              

              (testing "And now get-emails does not return removed email"
                (assert-that (get-emails contact)
                             (contains (has-slots 'address
                                                  "another@example.com")))))))))))


(deftest searching-by-email
    (testing
     "Contact can be searched by email"
     (with-empty-db
         (let ((contact (make-contact "Pupkin")))
           (add-email contact "some@example.com")

           (let ((search-results (find-contacts-by :email "some@example.com")))
             (assert-that search-results
                          (contains (has-slots 'hacrm/models/contact:name
                                               "Pupkin"))))))))


(deftest email-should-be-unique
    (testing
     "When email is added to the second contact, error should be raised"
     (with-empty-db
         (let ((contact (make-contact "Pupkin"))
               (second-contact (make-contact "Chubaka")))
           (add-email contact "some@example.com")

           (ok (signals (add-email second-contact "some@example.com")
                        'already-exists))))))


(deftest stripping-spaces-in-a-subject
    (testing
     "Check if mel-base strips first space from header value"
     (let ((result (mel:parse-rfc2822-header "Subject: hello   world")))
       (ok (equal result
                  '((:subject . "hello   world")))))))


(deftest unfolding-header-value
    (testing
     "Check if mel-base unfolds header value"
     (testing "Unfoldering should work for strings started from space"
              (let ((result (mel:parse-rfc2822-header "Subject: hello
 world")))
                (ok (equal result
                           '((:subject . "hello world"))))))

     (testing "And from tabs"
              (let* ((raw-header (concatenate 'string
                                              "Subject: hello"
                                              (coerce (list #\Return #\Linefeed #\Tab)
                                                      'string)
                                              "world"))
                     (expected-value (concatenate 'string
                                                  "hello"
                                                  (coerce (list #\Tab)
                                                          'string)
                                                  "world"))
                     (result (mel:parse-rfc2822-header raw-header)))
                (ok (equal result
                           `((:subject . ,expected-value))))))))


(deftest test-mel-consume
    (testing
     "Check consume function"
     (with-input-from-string (stream "   foo")
       (mel.mime::consume stream #\Space)
       (ok (eql (read-char stream)
                #\f)
           "First three spaces should be skipped and next char is 'f'."))))


;; Temporary test
(deftest test-email-parsing
    (testing
     "Check if email is parsed correctly"
     (set-password "xak40yw74f")
     (ng (ignore-errors
          (process-messages)))))
