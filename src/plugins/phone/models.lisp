(defpackage #:hacrm-phone/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/facts/core
                #:add-facts
                #:get-facts-of-type
                #:contact
                #:remove-facts
                #:fact
                #:fact-group
                #:deffact)
  (:import-from #:hacrm/models/core
                #:get-object-id
                #:get-root-object
                #:define-transaction
                #:find-object))
(in-package hacrm-phone/models)

(deffact phone
    ((number :type string
             :initarg :number
             :reader get-number)))


(defmethod print-object ((object phone) stream)
  (print-unreadable-object (object stream :type t)
    (princ (get-number object)
           stream)))


(defmethod fact-group ((fact phone))
  :phones)


(defun get-phones (contact)
  "Returns all phone bound to the contact."
  (get-facts-of-type contact 'phone))


(define-transaction tx-add-phone (contact-id number)
  (check-type contact-id integer)
  (check-type number string)
  (let ((phone (make-instance 'phone
                              :number number)))

    (add-facts (contact-id)
               phone)
    
    phone))


(defun add-phone (contact number)
  (check-type contact hacrm/models/contact:contact)
  (check-type number string)
  (execute-tx-add-phone (get-object-id contact)
                        number))


(define-transaction tx-remove-phone (contact-id number)
  (check-type contact-id integer)
  (check-type number string)

  (remove-facts (contact-id :type 'phone)
    (string-equal (get-number fact)
                  number)))


(defun remove-phone (contact number)
  (check-type contact hacrm/models/contact:contact)
  (check-type number string)
  (execute-tx-remove-phone (get-object-id contact)
                           number))
