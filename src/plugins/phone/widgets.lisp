(in-package asdf)
(defpackage #:hacrm.plugins.phone.widgets
  (:use #:cl
        #:cl-who
        #:weblocks))
(in-package hacrm.plugins.phone.widgets)


(defwidget phones ()
  ((contact :initarg :contact
            :reader contact)))


(defmethod hacrm.widgets.facts:make-facts-group-widget ((group (eql :phones))
                                                        contact)
  (declare (ignorable group))

  (make-instance 'phones
                 :contact contact))


(defmethod render-widget-body ((widget phones)
                               &rest args)
  (declare (ignorable args))

  (let* ((contact (contact widget))
         (phones (hacrm.plugins.phone:get-phones contact)))
    
    (with-html
      (:h1 "Phones")
      (:ul
       (dolist (phone phones)
         (htm (:li (esc (hacrm.plugins.phone:number phone)))))))))


