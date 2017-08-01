(defpackage #:hacrm.widgets.contacts-list
  (:use #:cl
        #:weblocks
        #:hacrm.models.contact
        #:f-underscore)
  (:import-from #:cl-who
                #:esc)
  (:export
   #:make-contacts-list
   #:contacts-list
   #:contacts
   #:render-facts))
(in-package hacrm.widgets.contacts-list)


(defwidget contacts-list (hacrm.widgets.base:base)
  ((contacts :initarg :contacts
             :reader contacts)
   (on-contact-click :type function
                     :initarg :on-contact-click
                     :reader on-contact-click)))


(defun make-contacts-list (contacts &key on-contact-click)
  (flet ((default-click-processor (contact)
           (log:info "No action was passed to process selection of the"
                     contact)))
    
    (make-instance 'contacts-list
                   :contacts contacts
                   :on-contact-click (or on-contact-click
                                         #'default-click-processor))))


(defgeneric render-facts (fact-group contact)
  (:documentation "Renders a closely related group of facts about a contact.
It is like concats-details:render-facts, but for a list view, so
some types of contacts may be hidden."))


(defun render (contact on-click)
  "Internal helper to render single contact in the contact's list."
  (with-html
    (:div :class "contact-list__contact"
          (:h1 (render-link on-click
                            (name contact)))
          (loop for fact-group in (hacrm.models.facts.core:fact-groups contact)
                do (render-facts fact-group contact)))))


(defmethod render-widget-body ((widget contacts-list) &rest args)
  (declare (ignore args))
  
  (let ((contacts (contacts widget))
        (on-click (on-contact-click widget)))

    (if contacts
        (flet ((make-click-processor (contact)
                 (f_%
                   (log:debug "Clicked" contact)
                   (funcall on-click contact))))
      
          ;; To not close over loop variable,
          ;; we use intermediate function generator make-click-processor.
          (loop for contact in contacts
                do (render contact (make-click-processor contact))))
        
        ;; No conctacts
        (with-html
          (:p "No contacts")))))
