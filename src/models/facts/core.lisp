(defpackage #:hacrm.models.facts.core
  (:use #:cl
        #:f-underscore)
  (:export
   #:fact
   #:deffact
   #:contact
   #:fact-groups
   #:find-contact-facts
   #:fact-group))
(in-package hacrm.models.facts.core)


(defclass fact ()
  ((id)
   (created-at :initform (get-universal-time)
               :reader created-at)
   (updated-at :initform (get-universal-time)
               :reader updated-at)
   (contact :initarg :contact
            :reader contact)))


(defmacro deffact (name slots)
  `(prog1
       (defclass ,name (fact)
         ,slots)
     
     (defmethod cl-prevalence::get-root-object ((system hacrm::hacrm-prevalence-system)
                                                (name (eql ',name)))
       (cl-prevalence::get-root-object system 'fact))


     (defmethod (setf cl-prevalence::get-root-object) (value
                                                       (system hacrm::hacrm-prevalence-system)
                                                       (name (eql ',name)))
       (setf (cl-prevalence::get-root-object system
                                             'fact)
             value))

     (defmethod weblocks-stores:find-persistent-objects :around
         ((store hacrm::hacrm-prevalence-system)
          (class-name (eql ',name)) 
          &key (filter nil) order-by range slot
            (value nil value-given-p)
            (test #'equal))
       "A wrapper to filter facts by they actual type."

       (log:debug "Searchin for per obj of" class-name)
       ;; TODO: unit-test it
       (let* ((filter (if filter
                          (f_ (and (typep _ ',name)
                                   (funcall filter _)))
                          (f_ (typep _ ',name))))
              (params (append (list store
                                    class-name
                                    :filter filter
                                    :order-by order-by
                                    :range range
                                    :slot slot
                                    :test test)
                              (when value-given-p
                                (list :value value)))))
         (apply #'call-next-method params)))))


(defgeneric fact-group (fact)
  (:documentation "This method should be defined for each `fact' subtype and return a keyword.

This keyword is used to group together similar facts like contacts or links to websites."))


(defun find-contact-facts (contact)
  (hacrm.utils:find-object
   'fact
   :filter (f_ (equal (weblocks-stores:object-id (contact _))
                      (weblocks-stores:object-id contact)))))


(defun fact-groups (contact)
  "Returns a list of keywords, denoting a groups of facts known about the contact."
  (let* ((facts (find-contact-facts contact))
         groups)
    (loop for fact in facts
          for group = (fact-group fact)
          do (pushnew group groups))

    groups))


