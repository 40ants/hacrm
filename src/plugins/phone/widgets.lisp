(defpackage #:hacrm/plugins/phone/widgets
  (:use #:cl))
(in-package hacrm/plugins/phone/widgets)


(weblocks/widget:defwidget phones ()
  ((contact :initarg :contact
            :reader contact)))


(defmethod hacrm/widgets/facts:make-facts-group-widget ((group (eql :phones))
                                                        contact)
  (declare (ignorable group))

  (make-instance 'phones
                 :contact contact))


(defmethod weblocks/widget:render ((widget phones))
  (let* ((contact (contact widget))
         (phones (hacrm/plugins/phone/models:get-phones contact)))
    
    (weblocks/html:with-html
      (:h1 "Phones")
      (:ul
       (dolist (phone phones)
         (:li (hacrm/plugins/phone/models:number phone)))))))


(defmethod weblocks/dependencies:get-dependencies ((widget phones))
  (list (weblocks-lass:make-dependency
         '(.phones
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 20px
               :margin-bottom 5px)
           (ul :list-style none
               :padding 0
               :margin 0
            (li :line-height 30px))))))
