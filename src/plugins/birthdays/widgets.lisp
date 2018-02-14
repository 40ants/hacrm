(defpackage #:hacrm-birthdays/widgets
  (:use #:cl)
  (:import-from #:hacrm-birthdays/models)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/widgets/facts
                #:fact-group-weight
                #:make-facts-group-widget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/dependencies
                #:get-dependencies))
(in-package hacrm-birthdays/widgets)


(defwidget birthday ()
  ((contact :initarg :contact
            :reader contact)
   (birthday :initarg :birthday
             :type hacrm-birthdays/models::birthday
             :reader birthday)))


(defmethod initialize-instance ((widget birthday) &rest args)
  (declare (ignorable args))

  (call-next-method)

  (let* ((contact (contact widget))
         (birthday (hacrm-birthdays/models::get-birthday contact)))

    (setf (slot-value widget 'birthday)
          birthday)))


(defmethod make-facts-group-widget ((group (eql :birthday))
                                    contact)
  (declare (ignorable group))

  (make-instance 'birthday
                 :contact contact))


(defmethod fact-group-weight ((widget birthday))
  "Birthday is important and should go right after tags."
  (declare (ignorable widget))
  1)


(defmethod render ((widget birthday))
  (with-html
    (:h1 "Birthday")
    (:p (hacrm-birthdays/models::date
         (birthday widget)))))



(defmethod get-dependencies ((widget birthday))
  (list (weblocks-lass:make-dependency
         '(.birthday
           (h1 :font-size 20px
               :line-height 30px
               :margin-top 10px
               :margin-bottom 5px)))))
