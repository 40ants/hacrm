(defpackage #:hacrm/plugins/tags/search
  (:use #:cl))
(in-package hacrm/plugins/tags/search)


(defmethod hacrm/search:index-facts ((fact-group (eql :tags))
                                     contact
                                     search-document)
  (let ((tags (hacrm/plugins/tags:get-contact-tags contact)))
    (dolist (tag tags)
      (montezuma:add-field
       search-document
       (montezuma:make-field
        "tag"
        (hacrm/plugins/tags:name tag))))

    (values)))
