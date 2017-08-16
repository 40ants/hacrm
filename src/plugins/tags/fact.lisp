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


(defun make-tag-fact (contact tag-name)
  (make-instance 'tag
                 :contact contact
                 :name tag-name))


(defun get-contact-tags (contact)
  (weblocks-stores:find-persistent-objects
   hacrm::*hacrm-store*
   'tag

   :filter (f_ (eql (weblocks-stores:object-id (contact _))
                    (weblocks-stores:object-id contact)))))

