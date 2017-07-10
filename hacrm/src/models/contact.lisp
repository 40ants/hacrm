(defpackage  #:hacrm.models.contact
  (:use #:cl)
  (:export #:contact
           #:make-contact
           #:name
           #:created
           #:find-contacts))
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
