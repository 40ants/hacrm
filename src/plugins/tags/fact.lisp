(in-package hacrm.plugins.tags)

(deffact new-tag
    ((name :type string
           :initarg :name
           :accessor name)))

(deffact tag
    ((name :type string
           :initarg :name
           :accessor name)))


(defmethod print-object ((fact tag) stream)
  (print-unreadable-object (fact stream :type t :identity t)
    (format stream "~a" (name fact))))


(defmethod fact-group ((fact tag))
  :tags)

(define-transaction tx-tag-contact (contact-id tag-name)
  (let* ((contact (first (hacrm.models.contact:find-contacts-by :id contact-id)))
         (tag (make-object 'tag
                           :contact contact
                           :name tag-name)))
    (push tag (get-root-object :facts))
    
    tag))


(defun tag-contact (contact tag-name &optional foo)
  (let ((tag (execute-tx-tag-contact (get-object-id contact)
                                     tag-name)))
    (weblocks.hooks:call-hook :fact-created contact tag)
    
    tag))


(defun get-contact-tags (contact)
  (hacrm.utils:find-object
   :facts
   :filter (f_ (and (typep _ 'tag)
                    (eql (contact _)
                         contact)))))

