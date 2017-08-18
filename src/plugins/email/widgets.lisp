(in-package asdf)
(defpackage #:hacrm.plugins.email.widgets
  (:use #:cl
        #:cl-who
        #:weblocks))
(in-package hacrm.plugins.email.widgets)


(defwidget emails ()
  ((contact :initarg :contact
            :reader contact)))


(defmethod hacrm.widgets.facts:make-facts-group-widget ((group (eql :emails))
                                                        contact)
  (declare (ignorable group))

  (make-instance 'emails
                 :contact contact))


(defmethod render-widget-body ((widget emails)
                               &rest args)
  (declare (ignorable args))

  (let* ((contact (contact widget))
         (emails (hacrm.plugins.email:get-emails contact)))
    
    (with-html
      (:h1 "Emails")
      (:ul
       (dolist (email emails)
         (htm (:li (esc (hacrm.plugins.email:address email)))))))))


