(defpackage #:hacrm/search
  (:use #:cl)
  (:import-from #:montezuma)
  (:import-from #:hacrm/widgets/main
                #:change-widget)
  (:import-from #:hacrm/widgets/contact-details
                #:make-contact-details-widget)
  (:import-from #:hacrm/widgets/contacts-list
                #:make-contacts-list)
  (:import-from #:hacrm/models/contact
                #:all-contacts
                #:get-name-synonyms)
  (:import-from #:hacrm/models/facts/core
                #:fact-groups)
  (:export
   #:index-contacts
   #:search-contacts
   #:index-facts))
(in-package hacrm/search)


(defvar *index* nil
  "Search index for contacts")

(defvar *next-document-id* 0)

(defvar *doc-by-id* (make-hash-table))


(defvar *experimental-index* nil
  "")


(defgeneric index-facts (fact-group contact search-document)
  (:documentation "Adds fields to a search document for each fact of given type.

If you want the facts added by your plugin were searchable, define this method.")

  (:method (fact-group contact search-document)
    (declare (ignorable fact-group contact search-document))))


(defclass hacrm-document (montezuma:document)
  ((object :initarg :object
           :reader get-object)))


(defun index-contacts ()
  (setf *index*
        (make-instance 'montezuma:index))

  (flet ((transform-to-document (contact)
           (let ((doc (make-instance 'hacrm-document
                                     :object contact)))
             (montezuma:add-field doc (montezuma:make-field
                                       "id"
                                       (format nil "~a" *next-document-id*)
                                       :index nil))
             ;; Now we'll put a reference to the indexed contact
             ;; to the table where we can get it by generated id:
             (setf (gethash *next-document-id* *doc-by-id*)
                   contact)
             (incf *next-document-id*)
             
             (dolist (name (get-name-synonyms contact))
               (montezuma:add-field doc (montezuma:make-field
                                         "name"
                                         name)))

             (loop for fact-group in (fact-groups contact)
                   do (index-facts fact-group contact doc))
             
             doc)))
    
    (loop for contact in (all-contacts)
          do (montezuma:add-document-to-index
              *index*
              (transform-to-document contact)))))


(defun rewrite-query (query)
  ;; For some reason, montezuma makes case insensitive search only
  ;; if query is in the lowercase.
  ;; But we want it alway be case insensitive.
  (let ((query (string-downcase query)))
    (if (find #\: query)
        query
        (format nil "name:~a* or tag:~a" query query))))


(defun search-contacts (query)
  (unless *index*
    (index-contacts))

  (let ((query (rewrite-query query)))
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
                      (cnt (gethash contact-id *doc-by-id*)))
                 cnt)))
        (mapcar #'to-contact results)))))


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


(defmethod hacrm/commands:command ((widget hacrm/widgets/base:base)
                                   (command (eql :search))
                                   query)
  "If no handler processed the query, then we'll try to search a contact."

  (log:debug "Trying to search contact" query)
  
  (let* ((contacts (search-contacts query))
         (contacts-count (length contacts)))
    (log:debug "Search completed" contacts-count)
    
    (cond
      ((eql contacts-count 1)
       (change-widget
        widget
        (make-contact-details-widget (car contacts))))
      (t
       (flet ((on-contact-selection (contact)
                (log:debug "Displaying contact" contact)
                (change-widget
                 widget
                 (make-contact-details-widget
                  contact))))
         (change-widget
          widget
          (make-contacts-list
           contacts
           :on-contact-click #'on-contact-selection)))))))
