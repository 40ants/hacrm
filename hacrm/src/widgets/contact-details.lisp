(defpackage #:hacrm.widgets.contact-details
  (:use #:cl
        #:cl-who
        #:weblocks
        #:f-underscore
        #:hacrm.models.contact)
  (:export :make-contact-details-widget
   :contact-details-contact
   :make-contact-details2-widget))
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


;; Second version

(defwidget contact-details2 ()
  ((contact :type 'contact
            :initarg :contact
            :reader get-contact)))


(defun make-contact-details2-widget (contact)
  (make-instance 'contact-details2
                 :contact contact))


(defmethod render-widget-body ((widget contact-details2) &rest args)
  (declare (ignorable args))

  (with-accessors ((contact get-contact))
      widget
    (let ((tags (hacrm.models.facts.tag:get-contact-tags contact)))
      (with-html
        (:h1 (esc (name contact)))

        (if tags
            (with-html
              (:p "Tags:")
              (:ul (loop for tag in tags
                         do (with-html (:li (esc (hacrm.models.facts.tag:name tag)))))))
            (with-html
              (:p "No details")))))))


(defmethod hacrm.query:process-query ((widget contact-details2)
                                      (token (eql :tag))
                                      query)
  (log:debug "Adding a tags from" query)
  
  (let* ((tokens (cl-strings:split query #\Space))
         (tags (cdr tokens)))
    (loop for tag in tags
          do (log:debug "Creating a tag" tag)
          do (hacrm.utils:store-object
              (hacrm.models.facts.tag:make-tag-fact
               (get-contact widget)
               tag))))

  (mark-dirty widget)
  (values))
