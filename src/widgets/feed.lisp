(defpackage #:hacrm.widgets.feed
  (:use #:cl
        #:weblocks
        #:cl-who
        #:f-underscore)
  (:export
   #:make-feed-widget
   #:make-feed-item-widget))
(in-package hacrm.widgets.feed)


(defwidget feed ()
  ((contact :initform nil
            :initarg :contact
            :reader contact)
   (items :initform nil
          :initarg :items
          :documentation "A list of widgets, representing feed items to display."
          :reader items)))


(defgeneric make-feed-item-widget (object parent-widget)
  (:documentation "This method should return a widget to represent given object in an activity feed."))


(defun make-feed-widget (contact)
  (let* ((relations (hacrm.models.relation:find-relation-from-object contact :type :activity))
         (objects (mapcar #'hacrm.models.relation:right relations))
         ;; feed items are sorted from recent to oldest
         (sorted-objects (sort objects
                               #'>
                               :key #'hacrm.models.feed:created-at))
         (feed-widget (make-instance 'feed :contact contact)))
    
    (let* ((items (mapcar (f_ (make-feed-item-widget _ feed-widget))
                          sorted-objects)))
      (setf (slot-value feed-widget 'items)
            items))

    feed-widget))


(defmethod render-widget-body ((widget feed) &rest args)
  (declare (ignorable args))
  
  (let ((items (items widget)))
    
    (with-html
      (:h1 "Активность")
      (dolist (item items)
        (render-widget item)))))


(defmethod weblocks.dependencies:get-dependencies  ((widget feed))
  (list (weblocks.lass:make-dependency
         '(.feed))))
