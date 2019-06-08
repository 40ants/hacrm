(defpackage #:hacrm-notes/models
  (:use #:cl
        #:f-underscore)
  ;; (:nicknames #:HACRM.PLUGINS.NOTES)
  (:import-from #:hacrm/models/feed
                #:get-feed-items
                #:add-feed-items
                #:remove-feed-items
                #:def-feed-item)
  (:import-from #:hacrm/models/core
                #:get-next-id
                #:get-object-id)
  (:import-from #:hacrm/utils
                #:first-line
                #:format-time)
  (:import-from #:hacrm/models/contact-utils
                #:find-contact-by)
  (:import-from #:hacrm/models/feed
                #:get-created-at)
  (:import-from #:prevalence-multimaster/system
                #:get-root-object)
  (:export
   #:get-text))
(in-package hacrm-notes/models)


(def-feed-item note
    ((text :type string
           :initarg :text
           :accessor get-text)))


(defmethod print-object ((note note) stream)
  (format stream "#<NOTE created=~A text=~S>"
          (format-time (get-created-at note))
          (first-line (get-text note))))


;; TODO: remove
(defun make-note (text)
  "Returns a new note with given text."
  (make-instance 'note :text text))


(prevalence-multimaster/transaction:define-transaction !add-note (contact-id new-fact-id text)
  (let* ((new-note (make-instance 'note
                                  :id new-fact-id
                                  :text text)))
    (add-feed-items (contact-id)
      new-note)
    
    new-note))


(prevalence-multimaster/transaction:define-transaction !remove-note (contact-id note-id)
  (let* ((contact (find-contact-by :id contact-id)))
    (remove-feed-items (note contact-id :type 'note)
      (equal (get-object-id note)
             note-id))))


(defun add-note (contact text)
  (check-type contact hacrm/models/contact:contact)
  (!add-note (get-object-id contact)
             (get-next-id)
             text))


(defun remove-note (contact note)
  (check-type contact hacrm/models/contact:contact)
  (check-type note note)
  (!remove-note (get-object-id contact)
                (get-object-id note)))


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
