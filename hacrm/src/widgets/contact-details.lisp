(defpackage #:hacrm.widgets.contact-details
  (:use #:cl
        #:cl-who
        #:weblocks
        #:f-underscore
        #:hacrm.models.contact)
  (:export :make-contact-details-widget
   :contact-details-contact
           :make-contact-details2-widget
   :contact-details2
           :get-contact
           :render-facts))
(in-package hacrm.widgets.contact-details)


(defwidget contact-details (hacrm.widgets.base:base)
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
          (:h1 (esc (hacrm.models.contact:name contact)))
          (render-link (f_% (setf (hacrm::current-contact contact-list)
                                  nil)
                            (mark-dirty contact-list))
                       "Отмена"
                       :class "btn btn-default")
          (render-widget (contact-details-notes widget)))
        (with-html
          (:p "Выберите какой-либо контакт")))))


;; Second version

(defwidget contact-details2 (hacrm.widgets.base:base)
  ((contact :type 'contact
            :initarg :contact
            :reader get-contact)))


(defun make-contact-details2-widget (contact)
  (make-instance 'contact-details2
                 :contact contact))


(defgeneric render-facts (fact-group contact)
  (:documentation "Renders a closely related group of facts about a contact."))


(defmethod render-widget-body ((widget contact-details2) &rest args)
  (declare (ignorable args))

  (with-accessors ((contact get-contact))
      widget
    (with-html
      (:h1 (esc (name contact)))

      (loop for fact-group in (hacrm.models.facts.core:fact-groups contact)
            do (render-facts fact-group contact)))))