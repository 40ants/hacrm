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


(define-condition already-exists (error)
  ((email :initarg :email
          :reader email)
   (other-contact :initarg :other-contact
                  :reader other-contact)))


(defun make-email-fact (contact email-address)
  (check-type email-address string)

  ;; First, check if this email address already bound to some
  ;; contact

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
  (let ((exists (hacrm.models.contact:find-contacts-by :email email-address)))
    (when exists
      (error 'already-exists))
    
    (hacrm.utils:store-object
     (make-email-fact contact email-address))))


(defun remove-email (contact email-to-remove)
  (check-type email-to-remove string)
  
  (let ((emails (get-emails contact))
        removed)
    (dolist (email emails)
      (when (string-equal (address email)
                          email-to-remove)
        (log:debug "Removing email from contact" email contact)
        (hacrm.utils:remove-object email)
        (push email removed)))

    removed))


(defmethod hacrm.models.contact:find-contacts-by ((keyword (eql :email)) value)
  (let* ((facts (hacrm.utils:find-object 'email
                                         :filter
                                         (f_ (string-equal (address _)
                                                           value))))
         (contact-id (when facts
                       (weblocks-stores:object-id (contact
                                                   (first facts)))))
         (all-contacts (hacrm.models.contact:all-contacts)))
    (hacrm.utils:find-object
     'hacrm.models.contact:contact
     :filter (f_ (equal (weblocks-stores:object-id _)
                        contact-id)))))
