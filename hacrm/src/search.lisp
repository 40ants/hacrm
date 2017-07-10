(defpackage #:hacrm.search
  (:use #:cl
        #:hacrm.models.contact)
  (:export
   #:index-contacts
   #:search-contact))
(in-package hacrm.search)


(defvar *index* nil
  "Search index for contacts")


(defvar *experimental-index* nil
  "")


(defun index-contacts ()
  (setf *index*
        (make-instance 'montezuma:index))

  (flet ((transform-to-document (contact)
           (let ((doc (make-instance 'montezuma:document))
                 (tags (hacrm.models.facts.tag:get-contact-tags contact)))
             (montezuma:add-field doc (montezuma:make-field
                                       "id"
                                       (format nil "~a"
                                               (slot-value contact
                                                           'hacrm.models.contact::id))
                                       :index nil))
             (montezuma:add-field doc (montezuma:make-field
                                       "name"
                                       (name contact)))

             ;; TODO: make this hookable, so new types of information
             ;; won't require to change this code.
             (loop for tag in tags
                   do (montezuma:add-field doc (montezuma:make-field
                                                "tag"
                                                (hacrm.models.facts.tag:name tag))))

             doc)))
    
    (loop for contact in (weblocks-stores:find-persistent-objects
                          hacrm::*hacrm-store*
                          'contact)
          do (montezuma:add-document-to-index
              *index*
              (transform-to-document contact)))))


(defun search-contact (query)
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
