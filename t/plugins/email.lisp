(defpackage #:hacrm.t.plugins.email
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:import-from #:hacrm.models.contact
                #:make-contact
                #:find-contacts-by)
  (:import-from #:hacrm.t.utils
                #:with-empty-db)
  (:import-from #:weblocks.t.utils
                #:with-session
                #:catch-hooks
                #:assert-hooks-called)
  (:import-from #:hacrm.plugins.email
                #:add-email
                #:remove-email
                #:get-emails
                #:address
                #:already-exists))
(in-package hacrm.t.plugins.email)


(plan 7)


(subtest "Email can be associated with a contact"
  (with-empty-db
    (with-session
      (catch-hooks (:fact-created :fact-removed)
        (let* ((contact (make-contact "Pupkin"))
               (some-email (add-email contact "some@example.com"))
               (another-email (add-email contact "another@example.com")))
          (subtest "Function add-email returns a fact object"
            (prove:is-type some-email 'hacrm.plugins.email:email))
          
          (subtest "Email creation calls :fact-created hook"
            (assert-hooks-called
             (:fact-created contact some-email)
             (:fact-created contact another-email)))

          (subtest "After email was added, it can be retrived with get-emails function"
            (assert-that (get-emails contact)
                         (contains (has-slots 'address
                                              "another@example.com")
                                   (has-slots 'address
                                              "some@example.com"))))

          (let ((removed-emails (remove-email contact "some@example.com")))

            (subtest "Function remove-email should return only removed facts"
              (assert-that removed-emails
                           (contains (has-slots 'address
                                                "some@example.com"))))

            (subtest "Email removal calls :fact-removed hook"
              (assert-hooks-called
               (:fact-created contact some-email)
               (:fact-created contact another-email)
               (:fact-removed contact (first removed-emails))))
            

            (subtest "And now get-emails does not return removed email"
              (assert-that (get-emails contact)
                           (contains (has-slots 'address
                                                "another@example.com"))))))))))


(subtest "Contact can be searched by email"
  (with-empty-db
    (let ((contact (make-contact "Pupkin")))
      (add-email contact "some@example.com")

      (let ((search-results (find-contacts-by :email "some@example.com")))
        (assert-that search-results
                     (contains (has-slots 'hacrm.models.contact:name
                                          "Pupkin")))))))


(subtest "When email is added to the second contact, error should be raised"
  (with-empty-db
    (let ((contact (make-contact "Pupkin"))
          (second-contact (make-contact "Chubaka")))
      (add-email contact "some@example.com")

      (is-condition (add-email second-contact "some@example.com")
                    'already-exists))))


(subtest "Check if mel-base strips first space from header value"
  (let ((result (mel:parse-rfc2822-header "Subject: hello   world")))
    (is result '((:subject . "hello   world")))))


(subtest "Check if mel-base unfolds header value"
  (subtest "Unfoldering should work for strings started from space"
    (let ((result (mel:parse-rfc2822-header "Subject: hello
 world")))
      (is result '((:subject . "hello world")))))

  (subtest "And from tabs"
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
      (is result `((:subject . ,expected-value))))))


(subtest "Check consume function"
  (with-input-from-string (stream "   foo")
    (mel.mime::consume stream #\Space)
    (subtest "First three spaces should be skipped and next char is 'f'."
      (is (read-char stream)
          #\f))))


(subtest "Check if email is parsed correctly"
  ;; Temporary test
  (hacrm.plugins.email.imap::set-password "xak40yw74f")
  (prove:isnt (ignore-errors
               (hacrm.plugins.email.imap::process-messages))
              nil))

(finalize)
