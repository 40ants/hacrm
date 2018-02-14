(defpackage #:hacrm-phone/widgets
  (:use #:cl)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/widgets/facts
                #:make-facts-group-widget)
  (:import-from #:hacrm-phone/models
                #:get-number
                #:number
                #:get-phones)
  (:import-from #:weblocks/dependencies
                #:get-dependencies))
(in-package hacrm-phone/widgets)


(defwidget phones ()
  ((contact :initarg :contact
            :reader contact)))


(defmethod make-facts-group-widget ((group (eql :phones))
                                    contact)
  (declare (ignorable group))

  (make-instance 'phones
                 :contact contact))


(defmethod render ((widget phones))
  (let* ((contact (contact widget))
         (phones (get-phones contact)))
    
    (weblocks/html:with-html
      (:h1 "Phones")
      (:ul
       (dolist (phone phones)
         (:li (get-number phone)))))))


(defmethod get-dependencies ((widget phones))
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
