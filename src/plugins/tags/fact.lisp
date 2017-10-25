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

(define-transaction tx-tag-contact (contact-id tag-name)
  (let* ((contact (hacrm.models.contact:find-contact-by-id contact-id))
         (tag (make-object 'tag
                           :contact contact
                           :name tag-name)))
    (push tag (get-root-object :facts))
    
    tag))


(defun tag-contact (contact tag-name)
  (let ((tag (execute-tx-tag-contact (get-object-id contact)
                                     tag-name)))
    (weblocks.hooks:call-hook :fact-created contact tag)
    
    tag))


(hacrm.models.core:define-transaction tx-untag-contact (contact-id tag-name)
  ;; First, we need to find a contact by id
  ;; Default let does not guarantee an order of execution
  ;; for binding clauses. That is why you need to replace it
  ;; with let*, if one binding depends on other
  ;; Now there is no warning after compilation.
  ;; By the way, I'm hitting C-c C-c each time, when I need
  ;; to compile a function.
  ;; Now it is loaded into the lisp image and we ready to check it with a
  ;; test.
  (let* ((contact (hacrm.models.contact:find-contact-by-id contact-id))
         ;; Tags are the facts which stored in a facts collection
         (all-facts (get-root-object :facts))
         ;; Now we need to filter-out facts, related to the contact and
         ;; having the given name
         (filtered-facts (remove-if (f_ (and (eql (contact _)
                                                  contact)
                                             (string-equal (name _)
                                                           tag-name)))
                                    all-facts)))
    ;; Now, filtered facts should be saved back to the collection
    (setf (get-root-object :facts)
          filtered-facts)))


(defun untag-contact (contact tag-name)
  ;; Ups, there is commented implementation,
  ;; copypasted from tag-contact :)

  ;; We need to call a cl-prevalence's transaction here
  ;; let's define it before untag-contact

  ;; Of cause, i forgot to call a transaction from the function!!!

  (execute-tx-untag-contact (get-object-id contact)
                            tag-name))


(defun get-contact-tags (contact)
  (hacrm.utils:find-object
   :facts
   :filter (f_ (and (typep _ 'tag)
                    (eql (contact _)
                         contact)))))

