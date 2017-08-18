(in-package hacrm.plugins.email)

(deffact email
    ((address :type string
            :initarg :address
            :reader address)))


(defmethod print-object ((object email) stream)
  (print-unreadable-object (object stream :type t)
    (princ (address object)
           stream)))


(defmethod fact-group ((fact email))
  :emails)


(defun make-email-fact (contact email-address)
  (check-type email-address string)

  (make-instance 'email
                 :address email-address
                 :contact contact))


(defun get-emails (contact)
  "Returns all email bound to the contact."
  (hacrm.utils:find-object
   'email
   :filter (f_ (equal (weblocks-stores:object-id (contact _))
                      (weblocks-stores:object-id contact)))))



(defun add-email (contact email-address)
  (hacrm.utils:store-object
   (make-email-fact contact email-address)))


(defun remove-email (contact email-to-remove)
  (check-type email-to-remove string)
  
  (let ((emails (get-emails contact))
        removed)
    (dolist (email emails)
      (when (string-equal (address email)
                          email-to-remove)
        (hacrm.utils:remove-object email)
        (push email removed)))

    removed))
