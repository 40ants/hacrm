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
   #:render-facts
   #:show-fact-group-in-contact-list-p))
(in-package hacrm.widgets.contacts-list)


(defwidget contact-card (hacrm.widgets.base:base)
  ((contact :type 'contact
            :initarg :contact
            :reader contact)
   (fact-groups :initarg :fact-groups
                :reader fact-groups)
   (on-click :type function
             :initarg :on-click
             :reader on-click)))


(defwidget contacts-list (hacrm.widgets.base:base)
  ((contacts :initarg :contacts
             :reader contacts)))


(defgeneric show-fact-group-in-contact-list-p (fact-group)
  (:documentation "Accepts a keyword denoting a fact group returns nil or t.

If nil is returned, then this kind of facts does not rendered in contact list
mode.

By default, nil is returned. If you really need to render the facts
in contact list mode, redefine this method.")
  (:method (fact-group)
    (declare (ignorable fact-group))
    nil))


(defun make-contact-card (contact on-click)
  (let* ((all-fact-groups (hacrm.models.facts.core:fact-groups
                       contact))
         (fact-groups (remove-if-not #'show-fact-group-in-contact-list-p
                                     all-fact-groups))
         (fact-group-widgets
           (mapcar
            (f_ (hacrm.widgets.facts:make-facts-group-widget
                 _
                 contact))
            fact-groups)))
    
    (make-instance 'contact-card
                   :contact contact
                   :on-click on-click
                   :fact-groups fact-group-widgets)))


(defun make-contacts-list (contacts &key on-contact-click)
  (flet ((default-click-processor (contact)
           (log:info "No action was passed to process selection of the"
                     contact)))
    
    (make-instance
     'contacts-list
     :contacts (mapcar
                (f_ (make-contact-card
                     _
                     (or on-contact-click
                         #'default-click-processor)
                     ))
                contacts))))


(defmethod render-widget-body ((widget contact-card) &rest rest)
  "Internal helper to render single contact in the contact's list."
  (declare (ignorable rest))
  
  (let ((contact (contact widget))
        (fact-group-widgets (fact-groups widget))
        (on-click-callback (on-click widget)))

    (with-html
      (:div :class "contact-list__contact"
            (:h1 (render-link (f_% (funcall on-click-callback
                                            contact))
                              (name contact)))

            (mapcar #'render-widget fact-group-widgets)))))


(defmethod render-widget-body ((widget contacts-list) &rest rest)
  (declare (ignorable rest))
  
  (let ((contacts (contacts widget)))

    (if contacts
        (mapcar #'render-widget
                contacts)
        
        ;; No contacts
        (with-html
          (:p "No contacts")))))
