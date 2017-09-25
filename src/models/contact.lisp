(defpackage  #:hacrm.models.contact
  (:use #:cl)
  (:export #:contact
           #:make-contact
           #:name
           #:created
           #:find-contacts
           #:get-name-synonyms))
(in-package hacrm.models.contact)


(defclass contact ()
  ((id)
   (name :type string
         :initarg :name
         :accessor name)
   (created :type integer
            :initform (get-universal-time)
            :reader created)))


(defun make-contact (name)
  "Создать карточку с контактом."
  (make-instance 'contact
                 :name name))


(defun find-contacts ()
  (weblocks-stores:find-persistent-objects
   hacrm::*hacrm-store*
   'contact))


(defmethod print-object ((contact contact) stream)
  (format stream "#<CONTACT ~S>"
          (name contact)))


(defparameter *name-synonyms*
  '(("саша" "александр" "шурик" "шура")
    ("алексей" "лёша" "леша" "алекс")
    ("юра" "юрий" "юрик")
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
