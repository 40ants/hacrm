(defpackage #:hacrm.models.facts.core
  (:use #:cl
        #:f-underscore)
  (:export
   #:fact
   #:deffact
   #:contact
   #:fact-groups))
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



(defun fact-groups (contact)
  "Returns a list of keywords, denoting a groups of facts known about the contact."
  (declare (ignorable contact))
  ;; TODO: make this list taken from a database
  (list :tags))
