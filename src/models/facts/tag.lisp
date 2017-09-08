;; (defpackage #:hacrm.models.facts.tag
;;   (:use #:cl
;;         #:cl-who
;;         #:hacrm.models.facts.core
;;         #:f-underscore)
;;   (:export
;;    #:make-tag-fact
;;    #:name
;;    #:tag
;;    #:get-contact-tags))
;; (in-package hacrm.models.facts.tag)


;; (deffact tag
;;     ((name :type string
;;            :initarg :name
;;            :accessor name)))


;; (defmethod print-object ((fact tag) stream)
;;   (print-unreadable-object (fact stream :type t :identity t)
;;     (format stream "~a" (name fact))))


;; (defun make-tag-fact (contact tag-name)
;;   (make-instance 'tag
;;                  :contact contact
;;                  :name tag-name))


;; (defun get-contact-tags (contact)
;;   (weblocks-stores:find-persistent-objects
;;    hacrm::*hacrm-store*
;;    'hacrm.models.facts.tag:tag

;;    :filter (f_ (and ;(typep _ 'hacrm.models.facts.tag:tag)
;;                     (eql (contact _)
;;                          contact)))))


;; (defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details)
;;                                    (keyword (eql :tag))
;;                                    query)
;;   (log:debug "Adding a tags from" query)
  
;;   (let* ((tokens (cl-strings:split query #\Space))
;;          (tags (cdr tokens)))
;;     (loop for tag in tags
;;           do (log:debug "Creating a tag" tag)
;;           do (hacrm.utils:store-object
;;               (hacrm.models.facts.tag:make-tag-fact
;;                (hacrm.widgets.contact-details:get-contact widget)
;;                tag))))

;;   (weblocks:mark-dirty widget)
;;   (hacrm.commands:reset-user-input widget)
;;   (hacrm.search:index-contacts)
;;   (values))


;; (defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details)
;;                                    (keyword (eql :untag))
;;                                    query)
;;   (log:debug "Removing tags from" query)
  
;;   (let* ((tokens (cl-strings:split query #\Space))
;;          (tags-to-remove (cdr tokens))
;;          (contact (hacrm.widgets.contact-details:get-contact widget))
;;          (contact-tags (get-contact-tags contact)))
    
;;     (loop for tag in tags-to-remove
;;           do (let ((tag-object (find tag
;;                                      contact-tags
;;                                      :test #'string=
;;                                      :key #'name)))
;;                (when tag-object
;;                  (log:debug "Removing" tag)
;;                  (hacrm.utils:remove-object tag-object)))))

;;   (weblocks:mark-dirty widget)
;;   (hacrm.commands:reset-user-input widget)
;;   (hacrm.search:index-contacts)
;;   (values))


;; (defmethod hacrm.widgets.contact-details:render-facts ((fact-group (eql :tags))
;;                                                        contact)
;;   (declaim (ignorable fact-group))

;;   (let ((tags (get-contact-tags contact)))
;;     (when tags
;;         (weblocks:with-html
;;           (:p "Tags:")
;;           (:ul (loop for tag in tags
;;                      do (weblocks:with-html (:li (esc (name tag))))))))))
