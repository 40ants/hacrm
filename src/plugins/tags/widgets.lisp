(defpackage #:hacrm.plugins.tags.widgets
  (:use #:cl
        #:weblocks
        #:cl-who
        #:f-underscore
        #:hacrm.plugins.tags))

(in-package hacrm.plugins.tags.widgets)


(defwidget tags ()
  ((object :initarg :object
           :reader object
           :type hacrm.models.contact:contact)))


(defmethod hacrm.widgets.facts:make-facts-group-widget ((fact-group (eql :tags))
                                                        object)
  (declare (ignorable fact-group))

  (make-instance 'tags
                 :object object))


(defmethod hacrm.widgets.facts:fact-group-weight ((widget tags))
  "Tags should be on the top, right under the name."
  (declare (ignorable widget))
  0)


(defmethod render-widget-body ((widget tags) &rest rest)
  (declare (ignorable rest))

  (let* ((contact (object widget))
         (tags (get-contact-tags contact))
         (tag-names (sort (mapcar #'name tags)
                          #'string-lessp))
         (tag-names-with-hash (mapcar (f_ (concatenate 'string
                                                       "#"
                                                       _))
                                      tag-names))
         (tags-string (cl-strings:join tag-names-with-hash :separator ", ")))
    (weblocks:with-html
      (:p (:span (esc tags-string))))))
