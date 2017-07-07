(defpackage #:hacrm.models.facts.core
  (:use #:cl)
  (:export
   #:fact
   #:deffact))
(in-package hacrm.models.facts.core)


(defclass fact ()
  ((id)
   (created-at :initform (get-universal-time)
               :reader created-at)
   (updated-at :initform (get-universal-time)
               :reader updated-at)
   (contact :initarg :contact
            :reader fact-contact)))


(defmacro deffact (name slots)
  `(prog1
       (defclass ,name (fact)
         ,slots)
     
     (defmethod cl-prevalence::get-root-object ((system hacrm::hacrm-prevalence-system)
                                                (name (eql ',name)))
       (cl-prevalence::get-root-object system 'hacrm.models.playground::fact))


     (defmethod (setf cl-prevalence::get-root-object) (value
                                                       (system hacrm::hacrm-prevalence-system)
                                                       (name (eql ',name)))
       (setf (cl-prevalence::get-root-object system
                                             'hacrm.models.playground::fact)
             value))))

