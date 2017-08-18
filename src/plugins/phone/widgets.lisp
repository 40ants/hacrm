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


(defmethod weblocks.dependencies:get-dependencies ((widget phones))
  (list (weblocks.lass:make-dependency
         '(.phones
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 20px
               :margin-bottom 5px)
           (ul :list-style none
               :padding 0
               :margin 0
            (li :line-height 30px))))))
