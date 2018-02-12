(defpackage #:hacrm/plugins/tags/widgets
  (:use #:cl
        #:f-underscore
        #:hacrm/plugins/tags/models))

(in-package hacrm/plugins/tags/widgets)


(weblocks/widget:defwidget tags ()
  ((object :initarg :object
           :reader object
           :type hacrm/models/contact:contact)))


(defmethod hacrm/widgets/facts:make-facts-group-widget ((fact-group (eql :tags))
                                                        object)
  (declare (ignorable fact-group))

  (make-instance 'tags
                 :object object))


(defmethod hacrm/widgets/contacts-list:show-fact-group-in-contact-list-p ((fact-group (eql :tags)))
  t)


(defmethod hacrm/widgets/facts:fact-group-weight ((widget tags))
  "Tags should be on the top, right under the name."
  (declare (ignorable widget))
  0)


(defmethod weblocks/widget:render ((widget tags))
  (let* ((contact (object widget))
         (tags (get-contact-tags contact))
         (tag-names (sort (mapcar #'name tags)
                          #'string-lessp))
         (tag-names-with-hash (mapcar (f_ (concatenate 'string
                                                       "#"
                                                       _))
                                      tag-names))
         (tags-string (cl-strings:join tag-names-with-hash :separator ", ")))
    (weblocks/html:with-html
      (:p (:span tags-string)))))
