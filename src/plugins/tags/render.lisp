(in-package hacrm.plugins.tags)


(defmethod hacrm.widgets.contact-details:render-facts ((fact-group (eql :tags))
                                                       contact)
  (declaim (ignorable fact-group))

  (log:debug "Rendering tags for" contact)
  
  (let ((tags (get-contact-tags contact)))
    (when tags
        (weblocks:with-html
          (:p "Tags:")
          (:ul (loop for tag in tags
                     do (weblocks:with-html (:li (esc (name tag))))))))))


(defmethod hacrm.widgets.contacts-list:render-facts ((fact-group (eql :tags))
                                                    contact)
  (declaim (ignorable fact-group))

  (log:debug "Rendering tags for" contact)

  (let* ((tags (get-contact-tags contact))
         (tag-names (mapcar #'name tags))
         (tags-string (cl-strings:join tag-names :separator ", ")))
    (weblocks:with-html
      (:p (:span "Tags: ")
          (:span (esc tags-string))))))
