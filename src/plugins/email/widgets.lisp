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
         (htm (:li (:a :href (concatenate 'string
                                          "mailto:"
                                          (hacrm.plugins.email:address email))
                       (esc (hacrm.plugins.email:address email))))))))))



(defmethod weblocks.dependencies:get-dependencies ((widget emails))
  (list (weblocks.lass:make-dependency
         '(.emails
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 20px
               :margin-bottom 5px)
           (ul :list-style none
               :padding 0
               :margin 0
            (li :line-height 30px))))))
