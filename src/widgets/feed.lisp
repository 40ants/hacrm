(defpackage #:hacrm/widgets/feed
  (:use #:cl
        #:f-underscore)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/models/core
                #:get-root-object)
  (:import-from #:hacrm/models/feed
                #:created-at
                #:related-to-object-p)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:export
   #:make-feed-widget
   #:make-feed-item-widget))
(in-package hacrm/widgets/feed)


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
  (let* ((all-feed-items ;; (get-root-object :feed-items)
           ;; TODO: replace with (get-feed contact)
           nil
           )
         (items-for-the-contact
           (remove-if-not (f_ (related-to-object-p
                               _
                               contact))
                          all-feed-items))
         ;; feed items are sorted from recent to oldest
         (sorted-objects (sort items-for-the-contact
                               #'>
                               :key #'created-at))
         (feed-widget (make-instance 'feed :contact contact)))
    
    (let* ((items (mapcar (f_ (make-feed-item-widget _ feed-widget))
                          sorted-objects)))
      (setf (slot-value feed-widget 'items)
            items))

    feed-widget))


(defmethod render ((widget feed))
  (let ((items (items widget)))
    
    (with-html
      (:h1 "Активность")
      (dolist (item items)
        (render item)))))


(defmethod get-dependencies  ((widget feed))
  (list (weblocks-lass:make-dependency
         '(.feed))))
