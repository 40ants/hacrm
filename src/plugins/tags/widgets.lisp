(defpackage #:hacrm-tags/widgets
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/contact)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/widgets/facts
                #:fact-group-weight
                #:make-facts-group-widget)
  (:import-from #:hacrm/widgets/contacts-list
                #:show-fact-group-in-contact-list-p)
  (:import-from #:cl-strings
                #:join)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:hacrm-tags/models
                #:name
                #:get-contact-tags))

(in-package hacrm-tags/widgets)


(defwidget tags ()
  ((object :initarg :object
           :reader object
           :type hacrm/models/contact:contact)))


(defmethod make-facts-group-widget ((fact-group (eql :tags))
                                    object)
  (declare (ignorable fact-group))

  (make-instance 'tags
                 :object object))


(defmethod show-fact-group-in-contact-list-p ((fact-group (eql :tags)))
  t)


(defmethod fact-group-weight ((widget tags))
  "Tags should be on the top, right under the name."
  (declare (ignorable widget))
  0)


(defmethod render ((widget tags))
  (let* ((contact (object widget))
         (tags (get-contact-tags contact))
         (tag-names (sort (mapcar #'name tags)
                          #'string-lessp))
         (tag-names-with-hash (mapcar (f_ (concatenate 'string
                                                       "#"
                                                       _))
                                      tag-names))
         (tags-string (join tag-names-with-hash :separator ", ")))
    (with-html
      (:p (:span tags-string)))))
