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
                #:reset-user-input
                #:change-widget)
  (:import-from #:hacrm/widgets/contact-details
                #:make-contact-details-widget)
  (:import-from #:hacrm/widgets/contacts-list
                #:make-contacts-list)
  (:import-from #:hacrm/widgets/help
                #:make-help-widget)
  (:import-from #:hacrm/search
                #:index-contacts
                #:search-contacts)
  (:import-from #:hacrm/widgets/version
                #:make-version-widget)
  (:import-from #:hacrm/debug
                #:start-slynk)
  (:import-from #:hacrm/desktop
                #:*window*))
(in-package hacrm/toplevel-commands)


;;; This package contains commands which can be executed on any screen

(defmethod command ((widget base)
                    (command (eql :add))
                    name)
  "Add a new contact."

  (log:debug "Adding new contact" name)
  (let ((contact (make-contact name)))
    (index-contacts)
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
           :on-contact-click #'on-contact-selection
           :search-query query)))))))


(defmethod command ((widget base)
                    (command (eql :quit))
                    query)
  "Quit from the program."
  (ceramic:quit))


(defmethod command ((widget base)
                    (command (eql :start-slynk))
                    query)
  "Starts slynk on a random port."
  (start-slynk)
  
  ;; After Slynk was started we'll show version information
  ;; so that user will know the port on which slynk is listening.
  (change-widget
   widget
   (make-version-widget)))


(defmethod command ((widget base)
                    (command (eql :open-devtools))
                    query)
  "Starts slynk on a random port."
  (ceramic:open-dev-tools *window*)
  (reset-user-input widget))
