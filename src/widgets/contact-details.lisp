(defpackage #:hacrm.widgets.contact-details
  (:use #:cl
        #:cl-who
        #:weblocks
        #:f-underscore
        #:hacrm.models.contact)
  (:export :make-contact-details-widget
   :contact-details-contact
           :make-contact-details2-widget
   :contact-details2
           :get-contact
           :render-facts))
(in-package hacrm.widgets.contact-details)


(defwidget contact-details2 (hacrm.widgets.base:base)
  ((contact :type 'contact
            :initarg :contact
            :reader get-contact)
   (feed :initform nil
         :reader contact-feed-widget)
   (fact-groups :initform nil
                :reader fact-groups)))


(defmethod initialize-instance ((details-widget contact-details2)
                                &key contact)
  (flet ((update-feed-widget ()
           (setf (slot-value details-widget 'feed)
                 (hacrm.widgets.feed:make-feed-widget contact))
           (mark-dirty details-widget))

         (update-fact-widgets ()
           "Creates widgets to render fact groups for the contact."
           (let* ((fact-groups (hacrm.models.facts.core:fact-groups contact))
                  (fact-group-widgets (mapcar (f_ (hacrm.widgets.facts:make-facts-group-widget
                                                   _
                                                   contact))
                                              fact-groups)))
             (setf (slot-value details-widget 'fact-groups)
                   fact-group-widgets))))
    
    ;; Create initial version of the feed widget
    (update-feed-widget)
    ;; Create widgets to render facts
    (update-fact-widgets)

    ;; Now we'll add hooks to update this widgets when something changed
    (weblocks.hooks:add-session-hook
     :feed-item-created
     (lambda (item)
       (declare (ignorable item))
       ;; TODO: add check if added item is related to the contact
       (update-feed-widget)))

    (weblocks.hooks:add-session-hook
     :fact-created
     (lambda (object fact)
       (declare (ignorable object fact))
       ;; TODO: add check if added item is related to the contact
       (update-fact-widgets))))

  
  (call-next-method))


(defun make-contact-details2-widget (contact)
  (make-instance 'contact-details2
                 :contact contact))


(defmethod render-widget-body ((widget contact-details2) &rest args)
  (declare (ignorable args))

  (with-accessors ((contact get-contact))
      widget
    (with-html
      (:table :class "contact"
              (:tr (:td :class "contact__details"
                        (:h1 (esc (name contact)))

                        (dolist (fact-group-widget (fact-groups widget))
                          (render-widget fact-group-widget)))
                   
                   (:td :class "contact__feed"
                        (render-widget (contact-feed-widget widget))))))))


(defmethod weblocks.dependencies:get-dependencies  ((widget contact-details2))
  (list (weblocks.lass:make-dependency
         '(.contact-details2
           (table :width 100%
            (td :vertical-align top)
            (.contact__details :width 30%
                               :border-right "1px solid gray")
            (.contact__feed :width 70%
                            :padding-left 10px))))))