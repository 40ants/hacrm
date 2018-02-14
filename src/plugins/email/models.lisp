(defpackage #:hacrm-email/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/facts/core
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
                #:find-contacts-by
                #:find-contact-by-id)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook))
(in-package hacrm-email/models)

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


(defun get-emails (contact)
  "Returns all emails bound to the contact."
  (find-object
   :facts
   :filter (f_ (and (typep _ 'email)
                    (equal (contact _)
                           contact)))))


(define-transaction tx-add-email (contact-id email-address)
  (check-type email-address string)
  (check-type contact-id integer)
  
  (let* ((contact (find-contact-by-id
                   contact-id))
         (email (make-object 'email
                             :address email-address
                             :contact contact)))
    (push
     email
     (get-root-object :facts))

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


(define-transaction tx-remove-email (contact-id email-to-remove)
  (check-type contact-id integer)
  (check-type email-to-remove string)

  (let* ((contact (find-contact-by-id contact-id))
         (emails (get-emails contact))
         removed)
    
    ;; Not optimal, but is ok for prototype
    (dolist (email emails)
      (when (string-equal (address email)
                          email-to-remove)
        (log:debug "Removing email from contact" email contact)
        (push email removed)))

    (setf (get-root-object :facts)
          (remove-if (f_ (member _ removed :test #'eql))
                     (get-root-object :facts)))

    removed))


(defun remove-email (contact email-to-remove)
  (let* ((contact-id (get-object-id contact))
         (removed-emails (execute-tx-remove-email contact-id
                                                  email-to-remove)))
    (dolist (removed-email removed-emails)
      (call-fact-removed-hook contact removed-email))

    removed-emails))


(defmethod find-contacts-by ((keyword (eql :email)) value)
  (let* ((facts (find-object :facts
                             :filter
                             (f_ (and (typep _ 'email)
                                      (string-equal (address _)
                                                    value))))))
    (loop for fact in facts
          collect (contact fact))))
