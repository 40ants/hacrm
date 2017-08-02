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


(defun make-feed-widget ()
  (make-instance 'feed))


(defgeneric render-feed-item (item)
  (:documentation "Renders a single feed item in the activity stream.

Each type of feed items should define a this method."))


(defmethod render-widget-body ((widget feed) &rest args)
  (declare (ignorable args))
  
  (let* (;;(contact (contact widget))
         (items (hacrm.utils:find-object 'hacrm.models.feed:feed-item)))
    
    (with-html
      (:h1 "Активность")
      (:ul (dolist (item items)
             (render-feed-item item))))))


(defmethod weblocks.dependencies:get-dependencies  ((widget feed))
  (list (weblocks.lass:make-dependency
         '(.feed))))
