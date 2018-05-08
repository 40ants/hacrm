(defpackage #:hacrm/toplevel-commands
  (:use #:cl)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/base
                #:base)
  (:import-from #:hacrm/models/contact
                #:get-all-contacts
                #:make-contact)
  (:import-from #:hacrm/widgets/main
                #:change-widget)
  (:import-from #:hacrm/widgets/contact-details
                #:make-contact-details-widget)
  (:import-from #:hacrm/widgets/contacts-list
                #:make-contacts-list)
  (:import-from #:hacrm/widgets/help
                #:make-help-widget)
  (:import-from #:hacrm/search
                #:search-contacts)
  (:import-from #:hacrm/widgets/version
                #:make-version-widget))
(in-package hacrm/toplevel-commands)


;;; This package contains commands which can be executed on any screen

(defmethod command ((widget base)
                    (command (eql :add))
                    name)
  "Add a new contact."

  (log:debug "Adding new contact" name)
  (let ((contact (make-contact name)))
    
    (change-widget
        widget
        (make-contact-details-widget contact))))


(defmethod command ((widget base)
                    (token (eql :all))
                    query)
  "Shows full contact list."
  (declare (ignorable query))

  (log:debug "Opening all contacts")
  
  (flet ((on-contact-selection (contact)
           (log:debug "Displaying contact" contact)
           (change-widget
            widget
            (make-contact-details-widget
             contact))))

    ;; TODO: разобраться, почему не срабатывает смена основного виджета
    (change-widget
     widget
     (make-contacts-list
      (get-all-contacts)
      :on-contact-click #'on-contact-selection))))



(defmethod command ((widget base)
                    (token (eql :help))
                    query)
  "Shows a list of available commands."
  (declare (ignorable query))

  (log:debug "Opening help for a widget")
  
  (change-widget
   widget
   (make-help-widget widget)))


(defmethod command ((widget base)
                    (token (eql :version))
                    query)
  "Shows a version of the program."
  (declare (ignorable query))

  (log:debug "Opening version widget")
  
  (change-widget
   widget
   (make-version-widget)))


(defmethod command ((widget base)
                    (command (eql :search))
                    query)
  "If no handler processed the query, then we'll try to search a contact."

  (log:debug "Trying to search contact" query)
  
  (let* ((contacts (search-contacts query))
         (contacts-count (length contacts)))
    (log:debug "Search completed" contacts-count)
    
    (cond
      ((eql contacts-count 1)
       (change-widget
        widget
        (make-contact-details-widget (car contacts))))
      (t
       (flet ((on-contact-selection (contact)
                (log:debug "Displaying contact" contact)
                (change-widget
                 widget
                 (make-contact-details-widget
                  contact))))
         (change-widget
          widget
          (make-contacts-list
           contacts
           :on-contact-click #'on-contact-selection)))))))
