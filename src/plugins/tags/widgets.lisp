(defpackage #:hacrm.plugins.tags.widgets
  (:use #:cl
        #:weblocks
        #:cl-who
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


(defmethod render-widget-body ((widget tags) &rest rest)
  (declare (ignorable rest))

  (let* ((contact (object widget))
         (tags (get-contact-tags contact))
         (tag-names (mapcar #'name tags))
         (tags-string (cl-strings:join tag-names :separator ", ")))
    (weblocks:with-html
      (:p (:span (esc (concatenate 'string
                                   "#"
                                   tags-string)))))))
