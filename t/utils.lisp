(defpackage #:hacrm.t.utils
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:export
   #:with-empty-db))
(in-package hacrm.t.utils)


;; For unittests we don't want to clutter console with debug information
(log:config :sane2 :warn)


(defun random-string (length)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (aref chars (random (length chars))))
            'string)))


(defmacro with-empty-db (&body body)
  `(let* ((*random-state* (make-random-state t))
          (prefix (concatenate 'string
                               (random-string 8)
                               "/")))
     (uiop:with-temporary-file (:pathname name :prefix prefix)
       (let* ((db-directory (uiop:pathname-directory-pathname name))
              (hacrm::*store* (hacrm::open-store db-directory))
              (hacrm::*transaction-log-length* 0)
              (hacrm::*log-transactions* t)
              (hacrm::*transactions* nil))
         ,@body))))


(plan 1)

(subtest "Checking if a separate database for unittests is created"
  (with-empty-db
    (is (length (hacrm.models.contact:all-contacts))
        0
        "Macro with-empty-db should reset database")
    (is (hacrm.models.core::get-next-id)
        1
        "And object ids should start from 1")))

(finalize)
