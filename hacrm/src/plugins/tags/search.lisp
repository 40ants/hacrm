(in-package hacrm.plugins.tags)


(defmethod hacrm.search:index-facts ((fact-group (eql :tags))
                        contact
                        search-document)
  (let ((tags (hacrm.plugins.tags:get-contact-tags contact)))
    (loop for tag in tags
          do (montezuma:add-field
              search-document
              (montezuma:make-field
               "tag"
               (hacrm.plugins.tags:name tag))))

    (values)))
