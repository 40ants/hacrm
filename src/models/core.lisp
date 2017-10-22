(defpackage #:hacrm.models.core
  (:use #:cl)
  (:export
   #:get-object-id
   #:base
;;   #:get-next-id
   #:define-transaction
   #:get-root-object
   #:make-object))
(in-package hacrm.models.core)


(defun tx-increment-max-id (store)
  (let ((previous (cl-prevalence:get-root-object store
                                                 :max-id)))
    (setf (cl-prevalence:get-root-object store
                                         :max-id)
          (+ (or previous 0)
             1))))


(defun get-next-id ()
  "Each object should have a unique id.
   This function updates an id counter in the database and return a new value."
  (unless hacrm::*hacrm-store*
    (error "Please, setup hacrm::*hacrm-store* first."))
  (cl-prevalence:execute-transaction
   (tx-increment-max-id hacrm::*hacrm-store*)))


(defclass base ()
  ((id :initarg :id
       :documentation "Every object should have unique id."
       :reader get-object-id)))


(defun make-object (class &rest args)
  "Use this function instead of make-instance, to created instances
of classes inherited from 'base. This helper also sets an unique
id for the object."
  
  (apply #'make-instance
         class
         :id (get-next-id)
         args))


(defmacro define-transaction (name (&rest args) &body body)
  "Defines a function to execute transaction and a function to call it.

Additional variable \"store\" will be bound to the current cl-prevalence
store during execution of the \"body\".

Also, a helper defined to call the transaction on hacrm::*hacrm-store*."
  
  (alexandria:with-gensyms (prevalence-system)
    `(progn (defun ,name (,prevalence-system ,@args)
              (macrolet  ((get-root-object (name)
                            `(cl-prevalence:get-root-object ,',prevalence-system
                                                            ,name)))
                ,@body))
          
            (defun ,(alexandria:symbolicate "EXECUTE-" name) (,@args)
              (cl-prevalence:execute-transaction
               (,name hacrm::*hacrm-store* ,@args))))))
