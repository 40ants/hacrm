(defpackage #:hacrm.t.utils
  (:use #:cl
        #:prove
        #:hamcrest.prove)
  (:export
   #:with-empty-db))
(in-package hacrm.t.utils)


(defun random-string (length)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length collect (aref chars (random (length chars))))
            'string)))


(defmacro with-empty-db (&body body)
  `(uiop:with-temporary-file (:pathname name :prefix (concatenate 'string
                                                                  (random-string 8)
                                                                  "/"))
     (let* ((db-directory (uiop:pathname-directory-pathname name))
            (hacrm::*hacrm-store* (make-instance 'hacrm::hacrm-prevalence-system
                                                 :directory db-directory)))
       ,@body)))


(with-empty-db
  (subtest "Checking if a separate database for unittests is created"
    (is (length (hacrm.models.contact:find-contacts))
        0)))
