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


(defun make-birthday-fact (contact date)
  (check-type date string)

  (let ((date (cl-strings:clean date)))
    ;; First, will check if it is correct date in the input.
    (handler-case (local-time:parse-timestring date)
      ;; If it is invalid, then we'll reraise our own
      ;; condition to not expose local-time's one.
      (local-time::invalid-timestring ()
        (error 'invalid-date-format
               :date date)))

    (make-instance 'birthday
                   :date date
                   :contact contact)))


(defun get-birthday (contact)
  (first
   (hacrm.utils:find-object
    'birthday
    :filter (f_ (equal (contact _)
                       contact)))))


(defun set-birthday (contact date)
  "Sets or updates a birthday for given contact.

Returns a new `birthday' fact."

  (let ((current (get-birthday contact)))
    (when current
      (hacrm.utils:remove-object current)))
  
  (hacrm.utils:store-object
   (make-birthday-fact
    contact
    date)))
