(defpackage #:hacrm.t.utils
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:export
   #:with-empty-db))
(in-package hacrm.t.utils)


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
              (hacrm::*hacrm-store* (make-instance 'hacrm::hacrm-prevalence-system
                                                   :directory db-directory)))
         ,@body))))


(subtest "Checking if a separate database for unittests is created"
  (with-empty-db
    (is (length (hacrm.models.contact:all-contacts))
        0)))
