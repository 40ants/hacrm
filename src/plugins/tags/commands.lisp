(in-package hacrm.plugins.tags)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details)
                                   (keyword (eql :tag))
                                   query)
  (log:debug "Adding a tags from" query)
  
  (let ((tags (cl-strings:split query #\Space)))
    (dolist (tag-name tags)
      (log:debug "Creating a tag" tag)
      
      (let* ((contact (hacrm.widgets.contact-details:get-contact widget)))
        (hacrm.plugins.tags:tag-contact contact tag-name))))

  (weblocks:mark-dirty widget)
  (hacrm.widgets.main:reset-user-input widget)
  (hacrm.search:index-contacts)
  (values))


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details)
                                   (keyword (eql :untag))
                                   query)
  (log:debug "Removing tags from" query)
  
  (let* ((tags-to-remove (cl-strings:split query #\Space))
         (contact (hacrm.widgets.contact-details:get-contact widget))
         (contact-tags (get-contact-tags contact)))
    
    (dolist (tag tags-to-remove)
      (let ((tag-object (find tag
                              contact-tags
                              :test #'string=
                              :key #'name)))
        (when tag-object
          (log:debug "Removing" tag)
          (hacrm.utils:remove-object tag-object)
          (weblocks.hooks:call-hook :fact-removed contact tag-object)))))

  (weblocks:mark-dirty widget)
  (hacrm.widgets.main:reset-user-input widget)
  (hacrm.search:index-contacts)
  (values))
