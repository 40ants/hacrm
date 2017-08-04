(defpackage #:hacrm.widgets.feed
  (:use #:cl
        #:weblocks
        #:cl-who)
  (:export
   #:make-feed-widget
   #:render-feed-item))
(in-package hacrm.widgets.feed)


(defwidget feed ()
  ((contact :initform nil
            :initarg :contact
            :reader contact)))


(defun make-feed-widget (contact)
  (make-instance 'feed :contact contact))


(defgeneric render-feed-item (item)
  (:documentation "Renders a single feed item in the activity stream.

Each type of feed items should define a this method."))


(defmethod render-widget-body ((widget feed) &rest args)
  (declare (ignorable args))
  
  (let* ((contact (contact widget))
         (relations (hacrm.models.relation:find-relation-from-object contact :type :activity))
         (items (mapcar #'hacrm.models.relation:right relations)))
    
    (with-html
      (:h1 "Активность")
      (:ul (dolist (item items)
             (render-feed-item item))))))


(defmethod weblocks.dependencies:get-dependencies  ((widget feed))
  (list (weblocks.lass:make-dependency
         '(.feed))))
