(defpackage #:hacrm/widgets/feed
  (:use #:cl
        #:f-underscore)
  (:export
   #:make-feed-widget
   #:make-feed-item-widget))
(in-package hacrm/widgets/feed)


(weblocks/widget:defwidget feed ()
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
  (let* ((all-feed-items (hacrm/models/core:get-root-object :feed-items))
         (items-for-the-contact
           (remove-if-not (f_ (hacrm/models/feed:related-to-object-p
                               _
                               contact))
                          all-feed-items))
         ;; feed items are sorted from recent to oldest
         (sorted-objects (sort items-for-the-contact
                               #'>
                               :key #'hacrm/models/feed:created-at))
         (feed-widget (make-instance 'feed :contact contact)))
    
    (let* ((items (mapcar (f_ (make-feed-item-widget _ feed-widget))
                          sorted-objects)))
      (setf (slot-value feed-widget 'items)
            items))

    feed-widget))


(defmethod weblocks/widget:render ((widget feed))
  (let ((items (items widget)))
    
    (weblocks/html:with-html
      (:h1 "Активность")
      (dolist (item items)
        (weblocks/widget:render item)))))


(defmethod weblocks/dependencies:get-dependencies  ((widget feed))
  (list (weblocks-lass:make-dependency
         '(.feed))))
