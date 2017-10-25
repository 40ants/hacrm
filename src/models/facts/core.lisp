(defpackage #:hacrm.models.facts.core
  (:use #:cl
        #:f-underscore)
  (:export
   #:fact
   #:deffact
   #:contact
   #:fact-groups
   #:find-contact-facts
   #:fact-group
   #:all-facts
   #:remove-facts))
(in-package hacrm.models.facts.core)


(defclass fact (hacrm.models.core:base)
  ((created-at :initform (get-universal-time)
               :reader created-at)
   (updated-at :initform (get-universal-time)
               :reader updated-at)
   (contact :initarg :contact
            :reader contact)))


(defmacro deffact (name slots)
  `(prog1
       (defclass ,name (fact)
         ,slots)
     
     ;; (defmethod cl-prevalence::get-root-object ((system hacrm::hacrm-prevalence-system)
     ;;                                            (name (eql ',name)))
     ;;   (cl-prevalence::get-root-object system 'fact))


     ;; (defmethod (setf cl-prevalence::get-root-object) (value
     ;;                                                   (system hacrm::hacrm-prevalence-system)
     ;;                                                   (name (eql ',name)))
     ;;   (setf (cl-prevalence::get-root-object system
     ;;                                         'fact)
     ;;         value))

     ;; (defmethod weblocks-stores:find-persistent-objects :around
     ;;     ((store hacrm::hacrm-prevalence-system)
     ;;      (class-name (eql ',name)) 
     ;;      &key (filter nil) order-by range slot
     ;;        (value nil value-given-p)
     ;;        (test #'equal))
     ;;   "A wrapper to filter facts by they actual type."

     ;;   (log:debug "Searchin for per obj of" class-name)
     ;;   ;; TODO: unit-test it
     ;;   (let* ((filter (if filter
     ;;                      (f_ (and (typep _ ',name)
     ;;                               (funcall filter _)))
     ;;                      (f_ (typep _ ',name))))
     ;;          (params (append (list store
     ;;                                class-name
     ;;                                :filter filter
     ;;                                :order-by order-by
     ;;                                :range range
     ;;                                :slot slot
     ;;                                :test test)
     ;;                          (when value-given-p
     ;;                            (list :value value)))))
     ;;     (apply #'call-next-method params)))
     ))


(defgeneric fact-group (fact)
  (:documentation "This method should be defined for each `fact' subtype and return a keyword.

This keyword is used to group together similar facts like contacts or links to websites."))


(defun find-contact-facts (contact)
  (hacrm.utils:find-object
   :facts
   :filter (f_ (equal (contact _)
                      contact))))


(defun fact-groups (contact)
  "Returns a list of keywords, denoting a groups of facts known about the contact."
  (let* ((facts (find-contact-facts contact))
         groups)
    (loop for fact in facts
          for group = (fact-group fact)
          do (pushnew group groups))

    groups))


(defun all-facts ()
  (cl-prevalence:get-root-object hacrm::*store* :facts))


(defun fix-facts-contacts ()
  "Чиним ссылки на контактах в фактах. Из-за weblocks-stores они местами попортились."
  ;; TODO: remove this function if everything is all righth

  (let ((contacts-by-id (make-hash-table))
        (contacts (hacrm.models.contact:all-contacts))
        (facts (all-facts)))
    
    (loop for contact in contacts
          do (setf (gethash (weblocks-stores:object-id contact)
                            contacts-by-id)
                   contact))

    (loop for fact in facts
          for fact-contact = (contact fact)
          for fact-contact-id = (weblocks-stores:object-id fact-contact)
          for real-contact = (gethash fact-contact-id contacts-by-id)
          when (not (equal fact-contact
                           real-contact))
            collect (list
                     :fact fact
                     :fact-contact fact-contact
                     :it-s-facts (hacrm.models.facts.core:find-contact-facts fact-contact)
                     :real-contact real-contact
                     :it-s-facts (hacrm.models.facts.core:find-contact-facts real-contact))
            and do
              (setf (slot-value fact 'contact)
                    real-contact))))


(defmacro remove-facts (contact-id &body rules)
  "Removes a fact or facts bound to a contact with given id.

A rules is an expression, evaluated to check if the fact should be removed.
For example:

\(remove-facts contact-id \(string-equal \(number fact\)
                                       phone-number\)\)

Will remove all phone numbers where number is equal to given.

Variables 'contact' and 'fact' are bound to a contact identified by contact-id,
and to checked fact during rules evaluation.
"
  (alexandria:with-gensyms (all-facts filtered-facts)
    `(let* ((contact (hacrm.models.contact:find-contact-by-id ,contact-id))
            ;; Tags are the facts which stored in a facts collection
            (,all-facts (hacrm.models.core:get-root-object :facts))
            ;; Now we need to filter-out facts, related to the contact and
            ;; having the given name
            (,filtered-facts (remove-if (lambda (fact)
                                         (and (eql (contact fact)
                                                   contact)
                                              ,@rules))
                                       ,all-facts)))
       ;; Now, filtered facts should be saved back to the collection
       (setf (hacrm.models.core:get-root-object :facts)
             ,filtered-facts))))

