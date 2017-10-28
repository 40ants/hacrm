(in-package hacrm.plugins.phone)

(deffact phone
    ((number :type string
             :initarg :number
             :reader number)))


(defmethod print-object ((object phone) stream)
  (print-unreadable-object (object stream :type t)
    (princ (number object)
           stream)))


(defmethod fact-group ((fact phone))
  :phones)


(defun get-phones (contact)
  "Returns all phone bound to the contact."
  (hacrm.utils:find-object
   :facts
   :filter (f_ (and
                (typep _ 'phone)
                (equal (contact _)
                       contact)))))


(define-transaction tx-add-phone (contact-id number)
  (check-type contact-id integer)
  (check-type number string)
  (let* ((contact (hacrm.models.contact:find-contact-by-id contact-id))
         (phone (make-instance 'phone
                               :contact contact
                               :number number)))
    (push phone (get-root-object :facts))

    phone))


(defun add-phone (contact number)
  (check-type contact hacrm.models.contact:contact)
  (check-type number string)
  (execute-tx-add-phone (get-object-id contact)
                        number))


(define-transaction tx-remove-phone (contact-id number)
  (check-type contact-id integer)
  (check-type number string)

  (remove-facts (contact-id :type 'phone)
    (string-equal (number fact)
                  number)))


(defun remove-phone (contact number)
  (check-type contact hacrm.models.contact:contact)
  (check-type number string)
  (execute-tx-remove-phone (get-object-id contact)
                           number))
