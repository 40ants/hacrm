(defpackage #:hacrm-notes/models
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/feed
                #:get-feed-items
                #:add-feed-items
                #:remove-feed-items
                #:def-feed-item)
  (:import-from #:hacrm/models/core
                #:get-root-object
                #:make-object
                #:get-object-id
                #:define-transaction)
  (:import-from #:hacrm/utils
                #:first-line
                #:format-time)
  (:import-from #:hacrm/models/contact-utils
                #:find-contact-by)
  (:import-from #:hacrm/models/feed
                #:get-created-at))
(in-package hacrm-notes/models)


(def-feed-item note
    ((text :type string
           :initarg :text
           :accessor text)))


(defmethod print-object ((note note) stream)
  (format stream "#<NOTE created=~A text=~S>"
          (format-time (get-created-at note))
          (first-line (text note))))


;; TODO: remove
(defun make-note (text)
  "Returns a new note with given text."
  (make-instance 'note :text text))


(define-transaction tx-add-note (contact-id text)
  (let* ((new-note (make-object 'note
                                :text text)))
    (add-feed-items (contact-id)
      new-note)
    
    new-note))


(define-transaction tx-remove-note (contact-id note-id)
  (let* ((contact (find-contact-by :id contact-id)))
    (remove-feed-items (contact)
      (f_ (equal (get-object-id _)
                 note-id)))))


(defun add-note (contact text)
  (check-type contact hacrm/models/contact:contact)
  (execute-tx-add-note (get-object-id contact)
                       text))


(defun remove-note (note)
  (check-type note note)
  (execute-tx-remove-note (get-object-id note)))


;; TODO: rename get-notes into get-contact-notes
(defun get-notes (object)
  (check-type object hacrm/models/contact:contact)
  
  (let* ((full-feed (get-feed-items object))
         (object-notes (remove-if-not (f_ (typep _ 'note))
                                      full-feed)))
    object-notes))


(defun get-contact-notes (contact)
  (get-notes contact))


(defun get-all-notes ()
  (let* ((all-feed-items (get-root-object :feed-items)))
    (remove-if-not (f_ (typep _ 'note))
                   all-feed-items)))
