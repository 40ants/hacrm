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
  (let* ((date (clean-date date))
         (contact (hacrm.models.contact:find-contact-by-id contact-id))
         (birthday (get-birthday contact)))
    (cond
      (birthday
       (setf (slot-value birthday 'date)
             date))
      (t (setf birthday (make-object 'birthday
                                     :date date
                                     :contact contact))
         (push birthday
               (hacrm.models.core:get-root-object :facts))))

    birthday))


(defun set-birthday (contact date)
  "Sets or updates a birthday for given contact.

Returns a new `birthday' fact."

  (execute-tx-set-birthday (hacrm.models.core:get-object-id contact)
                           date))
