(defpackage #:hacrm/models/core
  (:use #:cl
        #:f-underscore)
  ;; (:nicknames #:HACRM.MODELS.CORE)
  (:import-from #:hacrm/db
                #:*store*)
  (:import-from #:cl-prevalence
                #:execute-transaction)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:prevalence-multimaster/system
                #:get-root-object)
  (:export
   #:get-object-id
   #:base
   #:get-next-id
   #:define-transaction
   ;; #:make-object
   #:remove-object-by-id
   #:compare-ids
   #:ids=))
(in-package hacrm/models/core)


;; (defun tx-increment-max-id (store)
;;   (let ((previous (cl-prevalence:get-root-object store
;;                                                  :max-id)))
;;     (setf (cl-prevalence:get-root-object store
;;                                          :max-id)
;;           (+ (or previous 0)
;;              1))))

;; (defun get-root-object (name)
;;   (unless (member name (list :contacts
;;                              :max-id))
;;     (error "Root object with name \"~A\" aren't supported"
;;            name))
  
;;   (cl-prevalence:get-root-object *store*
;;                                  name))


;; (defun (setf get-root-object) (value name)
;;   (setf (cl-prevalence:get-root-object *store* name)
;;         value))

(defun get-next-id ()
  "Each object should have a unique id.
   This function updates an id counter in the database and return a new value."
  (unless *store*
    (error "Please, setup hacrm/db::*store* first."))

  ;; (make-v4-uuid)
  (uuid:format-as-urn nil (make-v4-uuid))
  ;; (let ((previous (get-root-object :max-id)))
  ;;   (setf (get-root-object :max-id)
  ;;         (+ (or previous 0)
  ;;            1)))
  )

(defun ids= (left right)
  (cond
    ((and (typep left 'uuid:uuid)
          (typep right 'uuid:uuid))
     (uuid:uuid= left right))
    (t (equal left right))))


(defclass base ()
  ((id :initarg :id
       :documentation "Every object should have unique id."
       :reader get-object-id)))


;; (prevalence-multimaster/transaction:define-transaction !make-object (class id args)
;;   (apply #'make-instance
;;          class
;;          :id id
;;          args))


;; (defun make-object (class &rest args)
;;   "Use this function instead of make-instance, to created instances
;; of classes inherited from 'base. This helper also sets an unique
;; id for the object."

;;   (!make-object class (get-next-id) args))


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


(defun find-object (root-object-name &key filter)
  (let ((objects (get-root-object root-object-name)))
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
