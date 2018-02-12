(defpackage #:hacrm/models/core
  (:use #:cl
        #:f-underscore)
  (:import-from #:hacrm/db
                #:*store*)
  (:import-from #:cl-prevalence
                #:execute-transaction)
  (:import-from #:alexandria
                #:symbolicate)
  (:export
   #:get-object-id
   #:base
;;   #:get-next-id
   #:define-transaction
   #:get-root-object
   #:make-object
   #:remove-object-by-id))
(in-package hacrm/models/core)


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
  (unless *store*
    (error "Please, setup hacrm/db::*store* first."))
  (execute-transaction
   (tx-increment-max-id *store*)))


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

Also, a helper defined to call the transaction on hacrm/db::*store*."
  
  (alexandria:with-gensyms (prevalence-system)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name (,prevalence-system ,@args)
         (declare (ignorable ,prevalence-system))

         ;; When cl-prevalence restores data from transaction log
         ;; our store can be still a nil and transaction expecting
         ;; it to be a prevalence store, will fail.
         (let ((*store* ,prevalence-system))
           ,@body))
          
       (defun ,(symbolicate "EXECUTE-" name) (,@args)
         (execute-transaction
          (,name *store* ,@args))))))


(defun get-root-object (name)
  (cl-prevalence:get-root-object *store*
                                 name))

(defun (setf get-root-object) (value name)
  (setf (cl-prevalence:get-root-object *store* name)
        value))


(defun find-object (root-object-name &key filter)
  (let ((objects (hacrm/models/core:get-root-object root-object-name)))
    (if filter
        (remove-if-not filter objects)
        objects)))


(defun remove-object-by-id (root-name object-id)
  "A helper to remove object by id from the root list object."
  (check-type root-name symbol)
  (check-type object-id integer)
  
  (setf (get-root-object root-name)
        (remove-if (f_ (eql (get-object-id _)
                            object-id))
                   (get-root-object root-name))))
