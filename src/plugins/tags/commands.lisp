(defpackage #:hacrm-tags/commands
  (:use #:cl)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/contact-details
                #:get-contact
                #:contact-details)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:hacrm-tags/models
                #:tag-contact
                #:untag-contact)
  (:import-from #:weblocks/widget
                #:update)
  (:import-from #:hacrm/widgets/main
                #:reset-user-input)
  (:import-from #:hacrm/search
                #:index-contacts))
(in-package hacrm-tags/commands)


(defmethod command ((widget contact-details)
                    (keyword (eql :tag))
                    query)
  "Add one or more tags to the contact."
  (log:debug "Adding a tags from" query)
  
  (let ((tags (split query #\Space)))
    (dolist (tag-name tags)
      (log:debug "Creating a tag" tag-name)
      
      (let* ((contact (get-contact widget)))
        (tag-contact contact tag-name))))

  (update widget)
  (reset-user-input widget)
  (index-contacts)
  (values))


(defmethod command ((widget contact-details)
                    (keyword (eql :untag))
                    query)
  "Remove one or more tags from the contact."
  (log:debug "Removing tags from" query)
  
  (let* ((tags-to-remove (split query #\Space))
         (contact (get-contact widget)))
    
    (dolist (tag tags-to-remove)
      (untag-contact contact tag)))

  (update widget)
  (reset-user-input widget)
  (index-contacts)
  (values))
