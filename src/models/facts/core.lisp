(defpackage #:hacrm/models/facts/core
  (:use #:cl
        #:f-underscore)
  ;; (:nicknames #:HACRM.MODELS.FACTS.CORE)
  (:import-from #:hacrm/models/core
                #:base
                ;; #:find-object
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
    `(let* ((,contact (find-contact-by :id ,contact-id)))

       (cond
         (,contact (let ((new-facts (list ,@facts)))
                     (dolist (fact new-facts)
                       (push fact
                             (slot-value ,contact
                                         'facts)))
                     (values new-facts)))
         (t (log:error "Contact not found" ,contact-id))))))


(defmacro remove-facts ((item-name contact-id &key type) &body rules)
  `(remove-list-items
       facts
       ,item-name
       (,contact-id :type ,type)
     ,@rules))


;;(weblocks.hooks:call-hook :fact-removed contact tag-object)


