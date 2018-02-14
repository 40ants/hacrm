(defpackage #:hacrm-tags/search
  (:use #:cl)
  (:import-from #:montezuma)
  (:import-from #:hacrm/search
                #:index-facts)
  (:import-from #:hacrm-tags/models
                #:name
                #:get-contact-tags))
(in-package hacrm-tags/search)


(defmethod index-facts ((fact-group (eql :tags))
                        contact
                        search-document)
  (let ((tags (get-contact-tags contact)))
    (dolist (tag tags)
      (montezuma:add-field
       search-document
       (montezuma:make-field
        "tag"
        (name tag))))

    (values)))
