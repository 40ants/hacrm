(defpackage #:hacrm.widgets.contacts-list
  (:use #:cl
        #:weblocks
        #:hacrm.models.contact)
  (:import-from #:cl-who
                #:esc)
  (:export
   #:make-contacts-list
   #:contacts-list
   #:contacts))
(in-package hacrm.widgets.contacts-list)


(defwidget contacts-list ()
  ((contacts :initarg :contacts
             :reader contacts)))


(defun make-contacts-list (contacts)
  (make-instance 'contacts-list
                 :contacts contacts))



(defun render (contact)
  "Internal helper to render single contact in the contact's list."
  (with-html
    (:div :class "contact-list__contact"
          (:h1 (esc (name contact)))
          (:p "Здесь будут теги и прочая контактная информация."))))


(defmethod render-widget-body ((widget contacts-list) &rest args)
  (declare (ignore args))
  
  (let ((contacts (contacts widget)))
    (loop for contact in contacts
          do (render contact))))
