(in-package asdf)
(defpackage #:hacrm.plugins.birthdays.widgets
  (:use #:cl
        #:cl-who
        #:weblocks))
(in-package hacrm.plugins.birthdays.widgets)


(defwidget birthday ()
  ((contact :initarg :contact
            :reader contact)
   (birthday :initarg :birthday
             :type hacrm.plugins.birthdays::birthday
             :reader birthday)))


(defmethod initialize-instance ((widget birthday) &rest args)
  (declare (ignorable args))

  (call-next-method)

  (let* ((contact (contact widget))
         (birthday (hacrm.plugins.birthdays:get-birthday contact)))

    (setf (slot-value widget 'birthday)
          birthday)))


(defmethod hacrm.widgets.facts:make-facts-group-widget ((group (eql :birthday))
                                                        contact)
  (declare (ignorable group))

  (make-instance 'birthday
                 :contact contact))


(defmethod render-widget-body ((widget birthday)
                               &rest args)
  (declare (ignorable args))

  (with-html
    (:h1 "Birthday")
    (:p (esc (hacrm.plugins.birthdays::date
              (birthday widget))))))


