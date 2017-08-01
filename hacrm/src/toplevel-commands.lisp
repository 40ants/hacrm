(defpackage #:hacrm.toplevel-commands
  (:use #:cl))
(in-package hacrm.toplevel-commands)


;;; This package contains commands which can be executed on any screen

(defmethod hacrm.commands:command ((widget hacrm.widgets.base:base)
                    (command (eql :add))
                    name)
  "Add a new contact."

  (log:debug "Adding new contact" name)
  (let ((contact (hacrm.models.contact:make-contact name)))
    (hacrm.utils:store-object contact)
    
    (hacrm.widgets.main:change-widget
        widget
        (hacrm.widgets.contact-details:make-contact-details2-widget contact))))


(defmethod hacrm.commands:command ((widget hacrm.widgets.base:base)
                                   (token (eql :all))
                                   query)
  "Shows full contact list."
  (declare (ignorable query))

  (log:debug "Opening all contacts")
  
  (flet ((on-contact-selection (contact)
           (log:debug "Displaying contact" contact)
           (hacrm.widgets.main:change-widget
            widget
            (hacrm.widgets.contact-details:make-contact-details2-widget
             contact))))

    ;; TODO: разобраться, почему не срабатывает смена основного виджета
    (hacrm.widgets.main:change-widget
     widget
     (hacrm.widgets.contacts-list:make-contacts-list
      (hacrm.models.contact:find-contacts)
      :on-contact-click #'on-contact-selection))))

