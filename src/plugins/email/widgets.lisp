(defpackage #:hacrm-email/widgets
  (:use #:cl)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/widgets/facts
                #:make-facts-group-widget)
  (:import-from #:hacrm-email/models
                #:address
                #:get-emails)
  (:import-from #:weblocks/dependencies
                #:get-dependencies))
(in-package hacrm-email/widgets)


(defwidget emails ()
  ((contact :initarg :contact
            :reader contact)))


(defmethod make-facts-group-widget ((group (eql :emails))
                                    contact)
  (declare (ignorable group))

  (make-instance 'emails
                 :contact contact))


(defmethod render ((widget emails))
  (let* ((contact (contact widget))
         (emails (get-emails contact)))

    (weblocks/html:with-html
      (:h1 "Emails")
      (:ul
       (dolist (email emails)
         (:li (:a :href (concatenate 'string
                                     "mailto:"
                                     (address email))
                  (address email))))))))



(defmethod get-dependencies ((widget emails))
  (list (weblocks-lass:make-dependency
         '(.emails
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 20px
               :margin-bottom 5px)
           (ul :list-style none
               :padding 0
               :margin 0
            (li :line-height 30px))))))
