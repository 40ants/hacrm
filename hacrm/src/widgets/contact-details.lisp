(defpackage #:hacrm.widgets.contact-details
  (:use #:cl
        #:cl-who
        #:weblocks
        #:f-underscore)
  (:export :make-contact-details-widget
           :contact-details-contact))
(in-package hacrm.widgets.contact-details)


(defwidget contact-details ()
  ((contact-list :initarg :contact-list
                 :reader contact-details-contact-list)
   (contact :initform nil
            :initarg :contact
            :accessor contact-details-contact)
   (notes :initform (hacrm.widgets.notes:make-notes-widget)
          :reader contact-details-notes)))


(defmethod (setf contact-details-contact) :after (contact (widget contact-details))
  "Устанавливает новый контакт."
  (setf (hacrm.widgets.notes:notes-contact
         (contact-details-notes widget))
        contact)
  (mark-dirty widget))


(defun make-contact-details-widget (contact-list &optional current-contact)
  (make-instance 'contact-details
                 :contact-list contact-list
                 :contact current-contact))


(defmethod render-widget-body ((widget contact-details) &rest args)
  (declare (ignorable args))
  
  (with-slots (contact-list contact) widget
    (if contact
        (with-html
          (:h1 (esc (hacrm.models.contact:contact-name contact)))
          (render-link (f_% (setf (hacrm::current-contact contact-list)
                                  nil)
                            (mark-dirty contact-list))
                       "Отмена"
                       :class "btn btn-default")
          (render-widget (contact-details-notes widget)))
        (with-html
          (:p "Выберите какой-либо контакт")))))
