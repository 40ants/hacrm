(defpackage #:hacrm/plugins/birthdays/widgets
  (:use #:cl))
(in-package hacrm/plugins/birthdays/widgets)


(weblocks/widget:defwidget birthday ()
  ((contact :initarg :contact
            :reader contact)
   (birthday :initarg :birthday
             :type hacrm/plugins/birthdays/models::birthday
             :reader birthday)))


(defmethod initialize-instance ((widget birthday) &rest args)
  (declare (ignorable args))

  (call-next-method)

  (let* ((contact (contact widget))
         (birthday (hacrm/plugins/birthdays/models:get-birthday contact)))

    (setf (slot-value widget 'birthday)
          birthday)))


(defmethod hacrm/widgets/facts:make-facts-group-widget ((group (eql :birthday))
                                                        contact)
  (declare (ignorable group))

  (make-instance 'birthday
                 :contact contact))


(defmethod hacrm/widgets/facts:fact-group-weight ((widget birthday))
  "Birthday is important and should go right after tags."
  (declare (ignorable widget))
  1)


(defmethod weblocks/widget:render ((widget birthday))
  (weblocks/html:with-html
    (:h1 "Birthday")
    (:p (hacrm/plugins/birthdays/models::date
         (birthday widget)))))



(defmethod weblocks/dependencies:get-dependencies ((widget birthday))
  (list (weblocks-lass:make-dependency
         '(.birthday
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 10px
               :margin-bottom 5px)))))
