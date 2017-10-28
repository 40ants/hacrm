(in-package hacrm.plugins.birthdays)

(deffact birthday
    ((date :type string
           :initarg :date
           :reader date)))


(defmethod print-object ((object birthday) stream)
  (print-unreadable-object (object stream :type t)
    (princ (date object)
           stream)))


(defmethod fact-group ((fact birthday))
  :birthday)


(define-condition invalid-date-format (error)
  ((date :type string
         :initarg :date
         :reader date)))


(defun clean-date (date)
  "Prepares a date and checks it's format.

Should be applied to the user input before storing it into the database."
  
  (check-type date string)

  (let ((date (cl-strings:clean date)))
    ;; First, will check if it is correct date in the input.
    (handler-case (local-time:parse-timestring date)
      ;; If it is invalid, then we'll reraise our own
      ;; condition to not expose local-time's one.
      (local-time::invalid-timestring ()
        (error 'invalid-date-format
               :date date)))

    date))


(defun get-birthday (contact)
  (first
   (hacrm.utils:find-object
    :facts
    :filter (f_ (and (typep _ 'birthday)
                     (equal (contact _)
                            contact))))))


(hacrm.models.core:define-transaction tx-set-birthday (contact-id date)
  "If a new fact was created, second returned value is \"true\"."
  (let* ((date (clean-date date))
         (contact (hacrm.models.contact:find-contact-by-id contact-id))
         (birthday (get-birthday contact))
         created)
    (cond
      ;; Fact already exists
      (birthday
       (setf (slot-value birthday 'date)
             date))
      ;; New fact should be created
      (t (setf birthday (make-object 'birthday
                                     :date date
                                     :contact contact)
               created t)
               
         (push birthday
               (hacrm.models.core:get-root-object :facts))))

    (values birthday created)))


(defun set-birthday (contact date)
  "Sets or updates a birthday for given contact.

Returns a new `birthday' fact."

  (let* ((contact-id (hacrm.models.core:get-object-id contact)))
    (multiple-value-bind (fact created)
        (execute-tx-set-birthday contact-id date)

      (weblocks.hooks:call-hook (if created
                                    :fact-created
                                    :fact-updated)
                                contact
                                fact)

      (values fact created))))
