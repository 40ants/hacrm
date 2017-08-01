(defpackage #:hacrm.search
  (:use #:cl
        #:hacrm.models.contact)
  (:export
   #:index-contacts
   #:search-contact
   #:index-facts))
(in-package hacrm.search)


(defvar *index* nil
  "Search index for contacts")


(defvar *experimental-index* nil
  "")


(defgeneric index-facts (fact-group contact search-document)
  (:documentation "Adds fields to a search document for each fact of given type."))


(defun index-contacts ()
  (setf *index*
        (make-instance 'montezuma:index))

  (flet ((transform-to-document (contact)
           (let ((doc (make-instance 'montezuma:document)))
             (montezuma:add-field doc (montezuma:make-field
                                       "id"
                                       (format nil "~a"
                                               (slot-value contact
                                                           'hacrm.models.contact::id))
                                       :index nil))
             (montezuma:add-field doc (montezuma:make-field
                                       "name"
                                       (name contact)))

             (loop for fact-group in (hacrm.models.facts.core:fact-groups contact)
                   do (index-facts fact-group contact doc))
             
             doc)))
    
    (loop for contact in (weblocks-stores:find-persistent-objects
                          hacrm::*hacrm-store*
                          'contact)
          do (montezuma:add-document-to-index
              *index*
              (transform-to-document contact)))))


(defun search-contact (query)
  (unless *index*
    (index-contacts))

  (log:debug "Running" query)
  
  (let (results)
    (montezuma:search-each *index*
                           query
                           (lambda (doc score)
                             (declare (ignorable score))
                             (push doc results)))
    (flet ((to-contact (doc-index)
             (let* ((doc (montezuma:get-document *index* doc-index))
                    (contact-id (cl-strings:parse-number
                                 (montezuma:document-value doc "id")))
                    (cnt (weblocks-stores:find-persistent-object-by-id
                          hacrm::*hacrm-store*
                          'hacrm.models.contact:contact
                          contact-id)))
               cnt)))
      (mapcar #'to-contact results))))


(defun index-experimental ()
  (setf *experimental-index*
        (make-instance 'montezuma:index))

  (montezuma:add-document-to-index
   *experimental-index*
   (let ((doc (make-instance 'montezuma:document)))
     (montezuma:add-field doc (montezuma:make-field "id" "1" :index nil))
     (montezuma:add-field doc (montezuma:make-field
                               "name"
                               "Вася"))
     (montezuma:add-field doc (montezuma:make-field
                               "tag"
                               "lisp"))
     (montezuma:add-field doc (montezuma:make-field
                               "tag"
                               "django")))))


(defun rewrite-query (query)
  ;; For some reason, montezuma makes case insensitive search only
  ;; if query is in the lowercase.
  ;; But we want it alway be case insensitive.
  (let ((query (string-downcase query)))
    (if (find #\: query)
        query
        (format nil "name:~a* or tag:~a" query query))))


(defmethod hacrm.commands:command ((widget hacrm.widgets.base:base)
                    (command (eql :search))
                    query)
  "If no handler processed the query, then we'll try to search a contact."

  (log:debug "Trying to search contact" query)
  
  (let* ((search-query (rewrite-query query))
         (contacts (hacrm.search:search-contact search-query))
         (contacts-count (length contacts)))
    (log:debug "Search completed" contacts-count)
    
    (cond
      ((eql contacts-count 1)
       (hacrm.widgets.main:change-widget
        widget
        (hacrm.widgets.contact-details:make-contact-details2-widget (car contacts))))
      (t
       (hacrm.widgets.main:change-widget
        widget
        (hacrm.widgets.contacts-list:make-contacts-list
         contacts))))))
