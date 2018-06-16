(defpackage #:hacrm/widgets/contact-details
  (:use #:cl
        #:f-underscore
        #:hacrm/models/contact)
  (:import-from #:hacrm/models/facts/core)
  (:import-from #:weblocks-lass)
  (:import-from #:hacrm/widgets/feed
                #:make-feed-widget)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget
                #:update)
  (:import-from #:hacrm/widgets/facts
                #:make-facts-group-widget)
  (:import-from #:hacrm/widgets/base
                #:base)
  (:import-from #:weblocks/hooks
                #:on-session-hook-fact-removed
                #:on-session-hook-fact-created
                #:on-session-hook-feed-item-created)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:export #:make-contact-details-widget
           #:contact-details-contact
           #:make-contact-details-widget
           #:contact-details
           #:get-contact
           #:render-facts))
(in-package hacrm/widgets/contact-details)


(defwidget contact-details (base)
  ((contact :type 'hacrm/models/contact::contact
            :initarg :contact
            :reader get-contact)
   (feed :initform nil
         :reader contact-feed-widget)
   (fact-groups :initform nil
                :reader get-fact-groups)))


(defmethod initialize-instance ((details-widget contact-details)
                                &key contact)
  (flet ((update-feed-widget (&key redraw-p)
           (setf (slot-value details-widget 'feed)
                 (make-feed-widget contact))
           (when redraw-p
             (update details-widget)))

         (update-fact-widgets ()
           "Creates widgets to render fact groups for the contact."
           (log:debug "Updating fact widgets.")

           (let* ((fact-groups (hacrm/models/facts/core:get-fact-groups contact))
                  (fact-group-widgets (mapcar (f_ (make-facts-group-widget
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
    (on-session-hook-feed-item-created
      handle-new-feed-item (item)
      
      (declare (ignorable item))
      ;; TODO: add check if added item is related to the contact
      (update-feed-widget :redraw-p t))

    (on-session-hook-fact-created
      handle-new-fact (object fact)
      
      (declare (ignorable object fact))
      ;; TODO: add check if added item is related to the contact
      (update-fact-widgets))

    (on-session-hook-fact-removed
      handle-removed-fact (object fact)
      
      (declare (ignorable object fact))
       ;; TODO: add check if added item is related to the contact
      (update-fact-widgets)))

  
  (call-next-method))


(defun make-contact-details-widget (contact)
  (let ((widget (make-instance 'contact-details
                               :contact contact)))
    widget))


(defmethod render ((widget contact-details))
  (with-accessors ((contact get-contact))
      widget
    (weblocks/html:with-html
      (:table :class "contact"
              (:tr (:td :class "contact__details"
                        (:h1 (get-name contact))

                        (dolist (fact-group-widget (get-fact-groups widget))
                          (render fact-group-widget)))
                   
                   (:td :class "contact__feed"
                        (render (contact-feed-widget widget))))))))


(defmethod get-dependencies  ((widget contact-details))
  (list (weblocks-lass:make-dependency
         '(.contact-details
           (table :width 100%
            (td :vertical-align top)
            (.contact__details :width 30%
                               :border-right "1px solid gray")
            (.contact__feed :width 70%
                            :padding-left 10px))))))
