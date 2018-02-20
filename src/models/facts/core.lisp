(defpackage #:hacrm/models/facts/core
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/models/core
                #:base
                ;; #:find-object
                ;; #:get-root-object
                )
  (:import-from #:alexandria
                #:with-gensyms)
  (:import-from #:hacrm/models/contact-utils
                #:remove-list-items
                #:find-contact-by)
  (:export
   #:get-fact-groups
   #:get-facts
   #:deffact
   #:add-facts
   #:get-facts-of-type
   #:get-created-at
   #:get-updated-at))
(in-package hacrm/models/facts/core)


(defclass fact (base)
  ((created-at :initform (get-universal-time)
               :initarg :created-at
               :reader get-created-at)
   (updated-at :initform (get-universal-time)
               :initarg :updated-at
               :reader get-updated-at)))


(defclass object-with-facts-mixin ()
  ((facts :type list
          :initform nil
          :initarg :facts
          :documentation "A list of facts about an object. Contains objects of classes, derived from `fact'."
          :reader get-facts)))


(defmacro deffact (name slots)
  `(prog1
       (defclass ,name (fact)
         ,slots)
     ))


(defgeneric get-fact-groups (object)
  (:documentation "Returns a list of keywords, denoting a groups of facts known about the contact."))


(defmethod get-fact-groups ((object t))
  (let* ((facts (get-facts object))
         groups)
    (loop for fact in facts
          for group = (fact-group fact)
          do (pushnew group groups))

    groups))


(defun get-facts-of-type (contact type)
  (loop for fact in (get-facts contact)
        when (typep fact type)
          collect fact))


(defmacro add-facts ((contact-id) &rest facts)
  "Adds given facts to the contact."
  (with-gensyms (contact)
    `(let* ((,contact (find-contact-by :id ,contact-id))
            (new-facts (list ,@facts)))

       (unless ,contact
         (error "Contact with id ~A not found" ,contact-id))
       
       (dolist (fact new-facts)
         (push fact
               (slot-value ,contact
                           'facts)))

       (values new-facts))))


(defmacro remove-facts ((contact-id &key type) &body rules)
  `(remove-list-items
       facts
       (,contact-id :type ,type)
     ,@rules))


(defmacro remove-facts-old ((contact-id &key type) &body rules)
  "Removes a fact or facts bound to a contact with given id.

A rules is an expression, evaluated to check if the fact should be removed.
For example:

\(remove-facts \(contact-id :type 'tag\)
     \(string-equal \(number fact\)
                     phone-number\)\)

Will remove all phone numbers where number is equal to given.

Variables 'contact' and 'fact' are bound to a contact identified by contact-id,
and to checked fact during rules evaluation.

Returns a list of removed facts.
"
  (with-gensyms (contact all-facts filtered-facts removed-facts)
    `(let* ((,contact (find-contact-by :id ,contact-id))
            ;; Tags are the facts which stored in a facts collection
            (,all-facts (get-facts ,contact))
            ;; Now we need to filter-out facts, related to the contact and
            ;; having the given name
            ,filtered-facts
            ,removed-facts)
       
       (dolist (fact ,all-facts)
         ;; TODO: it is possible to move
         ;; this rules construction to the macro level
         ;; to optimize performance
         (if (and (or (null ,type)
                      (typep fact ,type))
                  ,@rules)
             (push fact ,removed-facts)
             (push fact ,filtered-facts)))
       ;; Now, filtered facts should be saved back to the collection
       (setf (slot-value ,contact 'facts)
             (nreverse ,filtered-facts))

       ;; Return removed facts
       (nreverse ,removed-facts))))

;;(weblocks.hooks:call-hook :fact-removed contact tag-object)


