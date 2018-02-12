(defpackage #:hacrm/widgets/contact-details
  (:use #:cl
        #:f-underscore
        #:hacrm/models/contact)
  (:export #:make-contact-details-widget
           #:contact-details-contact
           #:make-contact-details-widget
           #:contact-details
           #:get-contact
           #:render-facts))
(in-package hacrm/widgets/contact-details)


(weblocks/widget:defwidget contact-details (hacrm/widgets/base:base)
  ((contact :type 'contact
            :initarg :contact
            :reader get-contact)
   (feed :initform nil
         :reader contact-feed-widget)
   (fact-groups :initform nil
                :reader fact-groups)))


(defmethod initialize-instance ((details-widget contact-details)
                                &key contact)
  (flet ((update-feed-widget (&key redraw-p)
           (setf (slot-value details-widget 'feed)
                 (hacrm/widgets/feed:make-feed-widget contact))
           (when redraw-p
             (weblocks/widget:update details-widget)))

         (update-fact-widgets ()
           "Creates widgets to render fact groups for the contact."
           (log:debug "Updating fact widgets.")
           
           (let* ((fact-groups (hacrm/models/facts/core:fact-groups contact))
                  (fact-group-widgets (mapcar (f_ (hacrm/widgets/facts:make-facts-group-widget
                                                   _
                                                   contact))
                                              fact-groups))
                  ;; Now we reorder widgets according to their weights.
                  ;; To change ordering, define fact-group-weight method
                  ;; for your widget.
                  (sorted-widgets (sort fact-group-widgets
                                        #'<
                                        :key #'hacrm/widgets/facts:fact-group-weight)))
             (setf (slot-value details-widget 'fact-groups)
                   sorted-widgets))))
    
    ;; Create initial version of the feed widget
    (update-feed-widget)
    ;; Create widgets to render facts
    (update-fact-widgets)

    ;; Now we'll add hooks to update this widgets when something changed
    (weblocks/hooks:on-session-hook-feed-item-created
        handle-new-feed-item (item)
      
      (declare (ignorable item))
      ;; TODO: add check if added item is related to the contact
      (update-feed-widget :redraw-p t))

    (weblocks/hooks:on-session-hook-fact-created
        handle-new-fact (object fact)
      
      (declare (ignorable object fact))
      ;; TODO: add check if added item is related to the contact
      (update-fact-widgets))

    (weblocks/hooks:on-session-hook-fact-removed
        handle-removed-fact (object fact)
      
      (declare (ignorable object fact))
       ;; TODO: add check if added item is related to the contact
      (update-fact-widgets)))

  
  (call-next-method))


(defun make-contact-details-widget (contact)
  (let ((widget (make-instance 'contact-details
                               :contact contact)))
    widget))


(defmethod weblocks/widget:render ((widget contact-details))
  (with-accessors ((contact get-contact))
      widget
    (weblocks/html:with-html
      (:table :class "contact"
              (:tr (:td :class "contact__details"
                        (:h1 (name contact))

                        (dolist (fact-group-widget (fact-groups widget))
                          (weblocks/widget:render fact-group-widget)))
                   
                   (:td :class "contact__feed"
                        (weblocks/widget:render (contact-feed-widget widget))))))))


(defmethod weblocks/dependencies:get-dependencies  ((widget contact-details))
  (list (weblocks-lass:make-dependency
         '(.contact-details
           (table :width 100%
            (td :vertical-align top)
            (.contact__details :width 30%
                               :border-right "1px solid gray")
            (.contact__feed :width 70%
                            :padding-left 10px))))))
