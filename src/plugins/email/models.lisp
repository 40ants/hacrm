(defpackage #:hacrm-email/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/facts/core
                #:get-facts-of-type
                #:remove-facts
                #:add-facts
                #:fact-group
                #:contact
                #:deffact)
  (:import-from #:hacrm/models/core
                #:define-transaction
                #:get-object-id
                #:get-root-object
                #:make-object
                #:find-object)
  (:import-from #:hacrm/models/contact
                #:find-contacts-by)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook))
(in-package hacrm-email/models)

(deffact email
    ((address :type string
            :initarg :address
            :reader get-address)))


(defmethod print-object ((object email) stream)
  (print-unreadable-object (object stream :type t)
    (princ (get-address object)
           stream)))


(defmethod fact-group ((fact email))
  :emails)


(define-condition already-exists (error)
  ((email :initarg :email
          :reader email)
   (other-contact :initarg :other-contact
                  :reader other-contact)))


(defun get-emails (contact)
  "Returns all emails bound to the contact."
  (get-facts-of-type contact 'email))


(define-transaction tx-add-email (contact-id email-address)
  (check-type email-address string)
  (check-type contact-id integer)
  
  (let* ((email (make-object 'email
                             :address email-address)))
    (add-facts (contact-id)
               email)

    email))


(defun add-email (contact email-address)
  (let ((exists (find-contacts-by :email email-address)))
    (when exists
      (error 'already-exists))

    (let ((fact (execute-tx-add-email
                 (get-object-id contact)
                 email-address)))

      (call-fact-created-hook contact fact)

      fact)))


(define-transaction tx-remove-email
    (contact-id email-to-remove) (check-type contact-id integer)
    (check-type email-to-remove string)
    
    (remove-facts (contact-id :type 'email)
      (f_ (string-equal (get-address _)
                        email-to-remove))))


(defun remove-email (contact email-to-remove)
  (let* ((contact-id (get-object-id contact))
         (removed-emails (execute-tx-remove-email contact-id
                                                  email-to-remove)))
    (dolist (removed-email removed-emails)
      (call-fact-removed-hook contact removed-email))

    removed-emails))


(defmethod find-contacts-by ((keyword (eql :email)) value)
  (flet ((has-this-email (contact)
           (loop for email in (get-emails contact)
                 when (string-equal (get-address email)
                                    value)
                   do (return-from has-this-email t))))
    (find-object :contacts
                 :filter
                 #'has-this-email)))
