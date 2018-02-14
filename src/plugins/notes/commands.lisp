(defpackage #:hacrm-notes/commands
  (:use #:cl)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/contact-details
                #:get-contact
                #:contact-details)
  (:import-from #:hacrm/widgets/main
                #:reset-user-input)
  (:import-from #:weblocks/hooks
                #:with-feed-item-created-hook)
  (:import-from #:hacrm-notes/models
                #:add-note))
(in-package hacrm-notes/commands)


(defmethod command ((widget contact-details)
                    (keyword (eql :note))
                    rest-text)
  "Add a note to the contact."
  
  (declare (ignorable keyword))

  (log:info "Adding a note for the contact")
  
  (let* ((contact (get-contact widget))
         (note (add-note contact rest-text)))

    (reset-user-input widget)
    
    (with-feed-item-created-hook (note))
    (values)))
