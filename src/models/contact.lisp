(defpackage  #:hacrm/models/contact
  ;; (:nicknames #:HACRM.MODELS.CONTACT)
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/core
                #:ids=
                #:base
                #:get-object-id
                #:get-next-id
                #:find-object
                #:define-transaction)
  (:import-from #:cl-strings
                #:join
                #:split)
  (:import-from #:hacrm/models/facts/core
                #:object-with-facts-mixin)
  (:import-from #:hacrm/models/contact-utils
                #:find-contacts-by)
  (:import-from #:hacrm/models/feed
                #:object-with-feed-mixin)
  (:import-from #:prevalence-multimaster/system
                #:get-root-object)
  (:export #:contact
           #:make-contact
           #:get-name-synonyms
           #:get-all-contacts
           #:get-name
           #:get-created
           #:delete-contact
           #:get-created-at))
(in-package hacrm/models/contact)


(defclass contact (object-with-facts-mixin
                   object-with-feed-mixin
                   base)
  ((name :type string
         :initarg :name
         :accessor get-name)
   (created-at :type integer
               :initform (get-universal-time)
               :initarg :created-at
               :reader get-created-at)))


(prevalence-multimaster/transaction:define-transaction !make-contact (id name)
  "Create a new contact."
  (let ((contact (make-instance 'contact
                                :id id
                                :name name)))
    (push contact
          (get-root-object :contacts))
    contact))


(defun make-contact (name)
  "Create a new contact."
  (let ((result (!make-contact (get-next-id)
                               name)))
    result))


(prevalence-multimaster/transaction:define-transaction !delete-contact (id)
  (setf (get-root-object :contacts)
        (remove-if
         (lambda (contact)
           (equal (get-object-id contact)
                  id))
         (get-root-object :contacts)))

  (values))


(defun delete-contact (id)
  "Delete a contact."
  (!delete-contact id))


(defun get-all-contacts ()
  (get-root-object :contacts))


(defmethod print-object ((contact contact) stream)
  (format stream "#<CONTACT ~S>"
          (get-name contact)))


(defparameter *name-synonyms*
  '(("саша" "александр" "шурик" "шура")
    ("алексей" "лёша" "леша" "алекс")
    ("юра" "юрий" "юрик")
    ("дима" "дмитрий" "димон" "димка")
    ("петя" "пётр" "петр")))


(defparameter *name-synonyms-hash*
  (make-hash-table :test 'equal))


(loop for bunch in *name-synonyms*
      do (loop for name in bunch
               do (setf (gethash name *name-synonyms-hash*)
                        bunch)))


(defgeneric get-name-synonyms (contact-or-name)
  (:documentation "Returns a list of strings where Contact's name
replaced with different synonyms."))


(defmethod get-name-synonyms ((contact contact))
  (get-name-synonyms (string-downcase (get-name contact))))


(defmethod get-name-synonyms ((name string))
  "Returns other variants of contact's name, like
Alexander, Sasha, Shura."
  (let* ((parts (split name #\Space))
         (parts2 (mapcar (lambda (item)
                           (gethash item
                                    *name-synonyms-hash*
                                    ;; default
                                    item))
                         parts)))
    (labels ((inner (collected current rest)
               (typecase current
                 ;; If item is a string
                 (string
                  (if rest
                      ;; then we either need to proceed further
                      (inner (cons current collected)
                             (first rest)
                             (rest rest))
                      ;; or return resulting name
                      (list (join (reverse (cons current collected))
                                  :separator " "))))
                 ;; If item is a list of different synonyms, then we need to
                 ;; fanout them into the multiple variants of the name
                 (list
                  (apply #'append (mapcar (lambda (item)
                             (inner collected
                                    item
                                    rest))
                           current))))))
      
      (inner nil
             (first parts2)
             (rest parts2)))))


(defmethod find-contacts-by ((keyword (eql :name)) value)
  (find-object :contacts
               :filter
               (f_ (string-equal (get-name _)
                                 value))))


(defmethod find-contacts-by ((keyword (eql :id)) value)
  (find-object :contacts
               :filter
               (f_ (ids= (get-object-id _)
                         value))))


(defun show-contacts-with-ids ()
  (loop for c in (get-root-object :contacts)
        do (format t "~A: ~A~%"
                   (get-name c)
                   (get-object-id c)))
  (values))
