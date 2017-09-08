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


(defun make-phone-fact (contact phone-number)
  (check-type phone-number string)

  (make-instance 'phone
                 :number phone-number
                 :contact contact))


(defun get-phones (contact)
  "Returns all phone bound to the contact."
  (hacrm.utils:find-object
   'phone
   :filter (f_ (equal (weblocks-stores:object-id (contact _))
                      (weblocks-stores:object-id contact)))))


(defun add-phone (contact phone-number)
  (hacrm.utils:store-object
   (make-phone-fact contact phone-number)))


(defun remove-phone (contact phone-to-remove)
  (check-type phone-to-remove string)
  
  (let ((phones (get-phones contact))
        removed)
    (dolist (phone phones)
      (when (string-equal (number phone)
                          phone-to-remove)
        (hacrm.utils:remove-object phone)
        (push phone removed)))

    removed))
