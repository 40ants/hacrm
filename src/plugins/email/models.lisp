(defpackage #:hacrm-email/models
  (:use #:cl
        #:f-underscore)
  ;; (:nicknames #:HACRM.PLUGINS.EMAIL)
  (:import-from #:hacrm/models/facts/core
                #:get-facts-of-type
                #:remove-facts
                #:add-facts
                #:fact-group
                #:contact
                #:deffact)
  (:import-from #:hacrm/models/core
                #:get-next-id
                #:get-object-id
                #:find-object)
  (:import-from #:hacrm/models/contact
                #:find-contacts-by)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook)
  (:export
   #:get-address))
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


(prevalence-multimaster/transaction:define-transaction !add-email (contact-id next-fact-id email-address)
  (check-type email-address string)
  (check-type contact-id integer)
  
  (let* ((email (make-instance 'email
                               :id next-fact-id
                               :address email-address)))
    (add-facts (contact-id)
               email)

    email))


(defun add-email (contact email-address)
  (let ((exists (find-contacts-by :email email-address)))
    (when exists
      (error 'already-exists))

    (let ((fact (!add-email
                 (get-object-id contact)
                 (get-next-id)
                 email-address)))

      (call-fact-created-hook contact fact)

      fact)))


(prevalence-multimaster/transaction:define-transaction !remove-email
    (contact-id email-to-remove) (check-type contact-id integer)
    (check-type email-to-remove string)
    
    (remove-facts (email contact-id :type 'email)
      (string-equal (get-address email)
                    email-to-remove)))


(defun remove-email (contact email-to-remove)
  (let* ((contact-id (get-object-id contact))
         (removed-emails (!remove-email contact-id
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
