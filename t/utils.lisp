(defpackage #:hacrm-test/utils
  (:use #:cl
        #:rove
        #:hamcrest/rove)
  ;; (:import-from :hacrm)
  (:import-from #:hacrm/db
                #:*transactions*
                #:*log-transactions*
                #:*transaction-log-length*
                #:open-store
                #:*store*)
  (:export
   #:with-empty-db))
(in-package hacrm-test/utils)


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
              (*store* (open-store db-directory))
              (*transaction-log-length* 0)
              (*log-transactions* t)
              (*transactions* nil))
         ,@body))))

