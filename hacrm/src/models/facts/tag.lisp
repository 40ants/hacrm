(defpackage #:hacrm.models.facts.tag
  (:use #:cl
        #:hacrm.models.facts.core
        #:f-underscore)
  (:export
   #:make-tag-fact
   #:name
   #:tag
   #:get-contact-tags))
(in-package hacrm.models.facts.tag)


(deffact tag
    ((name :type string
           :initarg :name
           :accessor name)))


(defmethod print-object ((fact tag) stream)
  (print-unreadable-object (fact stream :type t :identity t)
    (format stream "~a" (name fact))))


(defun make-tag-fact (contact tag-name)
  (make-instance 'tag
                 :contact contact
                 :name tag-name))


(defun get-contact-tags (contact)
  (weblocks-stores:find-persistent-objects
   hacrm::*hacrm-store*
   'hacrm.models.facts.tag:tag

   :filter (f_ (eql (contact _)
                    contact))))
