(in-package hacrm.plugins.tags)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :tag))
                                   query)
  (log:debug "Adding a tags from" query)
  
  (let ((tags (cl-strings:split query #\Space)))
    (loop for tag in tags
          do (log:debug "Creating a tag" tag)
          do (hacrm.utils:store-object
              (hacrm.plugins.tags:make-tag-fact
               (hacrm.widgets.contact-details:get-contact widget)
               tag))))

  (weblocks:mark-dirty widget)
  (hacrm.widgets.main:reset-user-input widget)
  (hacrm.search:index-contacts)
  (values))


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :untag))
                                   query)
  (log:debug "Removing tags from" query)
  
  (let* ((tags-to-remove (cl-strings:split query #\Space))
         (contact (hacrm.widgets.contact-details:get-contact widget))
         (contact-tags (get-contact-tags contact)))
    
    (loop for tag in tags-to-remove
          do (let ((tag-object (find tag
                                     contact-tags
                                     :test #'string=
                                     :key #'name)))
               (when tag-object
                 (log:debug "Removing" tag)
                 (hacrm.utils:remove-object tag-object)))))

  (weblocks:mark-dirty widget)
  (hacrm.widgets.main:reset-user-input widget)
  (hacrm.search:index-contacts)
  (values))
