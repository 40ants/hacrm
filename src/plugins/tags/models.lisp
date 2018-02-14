(defpackage #:hacrm-tags/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/hooks)
  (:import-from #:hacrm/models/facts/core
                #:fact-group
                #:contact
                #:remove-facts
                #:fact
                #:deffact)
  (:import-from #:hacrm/models/core
                #:find-object
                #:get-root-object
                #:make-object
                #:get-object-id
                #:define-transaction)
  (:import-from #:hacrm/models/contact
                #:find-contact-by-id)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook))
(in-package hacrm-tags/models)

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
  (let* ((contact (find-contact-by-id contact-id))
         (tag (make-object 'tag
                           :contact contact
                           :name tag-name)))
    (push tag (get-root-object :facts))
    
    tag))


(defun tag-contact (contact tag-name)
  (let ((tag (execute-tx-tag-contact (get-object-id contact)
                                     tag-name)))
    (call-fact-created-hook contact tag)
    
    tag))


(define-transaction tx-untag-contact (contact-id tag-name)
  (remove-facts (contact-id :type 'tag)
    (string-equal (name fact)
                  tag-name)))


(defun untag-contact (contact tag-name)
  (let ((removed-tags (execute-tx-untag-contact (get-object-id contact)
                                                tag-name)))

    (dolist (tag removed-tags)
      (call-fact-removed-hook contact tag))))


(defun get-contact-tags (contact)
  (find-object
   :facts
   :filter (f_ (and (typep _ 'tag)
                    (eql (contact _)
                         contact)))))

(defun get-all-tags ()
  (find-object
   :facts
   :filter (f_ (typep _ 'tag))))

