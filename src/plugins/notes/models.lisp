(defpackage #:hacrm/plugins/notes/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/feed
                #:def-feed-item
                #:related-to-object-p)
  (:import-from #:hacrm/models/core
                #:get-root-object
                #:make-object
                #:get-object-id
                #:define-transaction))
(in-package hacrm/plugins/notes/models)


(def-feed-item note
    ((object :type hacrm/models/contact
             :initarg :object
             :accessor get-object)
     (text :type string
           :initarg :text
           :accessor text)))


(defmethod print-object ((note note) stream)
  (format stream "#<NOTE created=~A object=~A text=~S>"
          (hacrm/utils:format-time (created-at note))
          (when (slot-boundp note 'object)
            (get-object note))
          (hacrm/utils:first-line (text note))))


(defmethod related-to-object-p ((note note)
                                                  object)
  (eql (get-object note)
       object))


;; TODO: remove
(defun make-note (text)
  "Returns a new note with given text."
  (make-instance 'note :text text))


(hacrm/models/core:define-transaction tx-add-note (contact-id text)
  (let* ((contact (hacrm/models/contact:find-contact-by-id contact-id))
         (new-note (when contact
                     (make-object 'note
                                  :object contact
                                  :text text))))
    (when new-note
      (push new-note
            (get-root-object :feed-items)))
    
    new-note))


(define-transaction tx-remove-note (note-id)
  (hacrm/models/core:remove-object-by-id :feed-items note-id))


(defun add-note (contact text)
  (check-type contact hacrm/models/contact:contact)
  (execute-tx-add-note (get-object-id contact)
                       text))


(defun remove-note (note)
  (check-type note note)
  (execute-tx-remove-note (get-object-id note)))


;; TODO: rename get-notes into get-contact-notes
(defun get-notes (contact)
  (check-type contact hacrm/models/contact:contact)
  
  (let* ((full-feed (get-root-object :feed-items))
         (contact-notes (remove-if-not (f_ (and (typep _ 'note)
                                            (eql (get-object _)
                                                 contact)))
                                       full-feed)))
    contact-notes))

(defun get-contact-notes (contact)
  (get-notes contact))

(defun get-all-notes ()
  (let* ((all-feed-items (get-root-object :feed-items)))
    (remove-if-not (f_ (typep _ 'note))
                   all-feed-items)))
