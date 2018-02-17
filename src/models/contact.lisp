(defpackage  #:hacrm/models/contact
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/core
                #:base
                #:get-object-id
                #:find-object
                #:define-transaction
                #:make-object
                #:get-root-object)
  (:import-from #:cl-strings
                #:join
                #:split)
  (:import-from #:hacrm/models/facts/core
                #:object-with-facts-mixin)
  (:import-from #:hacrm/models/contact-utils
                #:find-contacts-by)
  (:export #:contact
           #:make-contact
           #:name
           #:created
           #:get-name-synonyms
           #:all-contacts))
(in-package hacrm/models/contact)


(defclass contact (object-with-facts-mixin base)
  ((name :type string
         :initarg :name
         :accessor name)
   (created :type integer
            :initform (get-universal-time)
            :reader created)))


(define-transaction tx-make-contact (name)
  (let ((contact (make-object 'contact
                              :name name)))
    (push contact
          (get-root-object :contacts))
    contact))


(defun make-contact (name)
  "Создать карточку с контактом."
  (execute-tx-make-contact name))


(defun all-contacts ()
  (get-root-object :contacts))


(defmethod print-object ((contact contact) stream)
  (format stream "#<CONTACT ~S>"
          (name contact)))


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
  (get-name-synonyms (string-downcase (name contact))))


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
               (f_ (string-equal (name _)
                                 value))))
