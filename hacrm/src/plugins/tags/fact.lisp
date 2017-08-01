(in-package hacrm.plugins.tags)

(deffact new-tag
    ((name :type string
           :initarg :name
           :accessor name)))


(defmethod print-object ((fact new-tag) stream)
  (print-unreadable-object (fact stream :type t :identity t)
    (format stream "~a" (name fact))))


(defun make-tag-fact (contact tag-name)
  (make-instance 'new-tag
                 :contact contact
                 :name tag-name))


(defun get-contact-tags (contact)
  (weblocks-stores:find-persistent-objects
   hacrm::*hacrm-store*
   'new-tag

   :filter (f_ (eql (weblocks-stores:object-id (contact _))
                    (weblocks-stores:object-id contact)))))

