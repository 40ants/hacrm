(defpackage  #:hacrm.models.contact
  (:use #:cl)
  (:import-from #:hacrm.models.core
                #:define-transaction
                #:make-object
                #:get-root-object)
  (:export #:contact
           #:make-contact
           #:name
           #:created
           #:find-contacts-by
           #:get-name-synonyms
           #:all-contacts
           #:find-contacts-by-id
           #:find-contact-by-id))
(in-package hacrm.models.contact)


(defclass contact (hacrm.models.core:base)
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
  (cl-prevalence:get-root-object hacrm::*hacrm-store* :contacts))


(defgeneric find-contacts-by (keyword value)
  (:documentation "Searches contacts by different facts, for example, by email, or twitter nickname.

Plugins should define methods for this generic, if they want to give ability to search
contacts by some associated data."))


(defun find-contact-by-id (value)
  (dolist (contact (all-contacts))
    (when (eql (hacrm.models.core:get-object-id contact)
               value)
      (return-from find-contact-by-id
        contact))))


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
  (let* ((parts (cl-strings:split name #\Space))
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
                      (list (cl-strings:join (reverse (cons current collected))
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
