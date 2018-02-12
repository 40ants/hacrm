(defpackage #:hacrm/db
  (:use #:cl)
  (:import-from #:log)
  (:import-from #:cl-prevalence
                #:snapshot
                #:get-transaction-hook
                #:get-guard
                #:guarded-prevalence-system))
(in-package hacrm/db)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.

(defvar *store* nil
  "Main database for all items operated by CRM.")


(defvar *default-db-path*
  #P "~/.hacrm/db/"
  "This path is used for production data.")

(defvar *dev-db-path*
  #P"~/.hacrm/dev-db/"
  "This path is used for development"
  )


(defvar *transaction-log-length* 0
  "Here we count a number of executed transactions to issue
   cl-prevalence:snapshot from time to time.

   Each time when execute-tx-something get called, this counter
   is incremented.")


(defvar *log-transactions* nil
  "This variable is set to True during unittests to debug transactions.")


(defvar *transactions* nil
  "This list stores transactions executed during hacrm.t.utils:with-empty-db's body.")


(defclass hacrm-prevalence-system (guarded-prevalence-system)
  ())


(defun open-store (path &rest args)
  (let* ((store (apply #'make-instance 'hacrm-prevalence-system :directory path args))
         (lock-name (format nil "Prevalence lock for store ~S" store))
         (lock (bordeaux-threads:make-lock lock-name)))
    ;; (setf (gethash store weblocks-prevalence::*locks*) lock)
    (setf (get-guard store)
          (lambda (thunk)
            (bordeaux-threads:with-lock-held (lock)
              (funcall thunk))))

    (setf (get-transaction-hook store)
          (lambda (transaction)
            (log:info "Executing transaction" transaction)
            (incf *transaction-log-length*)
            
            (when *log-transactions*
              (push transaction
                    *transactions*))))

    store))


(defun close-store (store)
  (snapshot store))



;; (defun get-objects-of-class (store class)
;;   (let* ((root (slot-value store 'root-objects))
;;          (objects1 (gethash class root))
;;          (objects2
;;            (slot-value objects1 'weblocks-prevalence::objects-by-id)))
;;     (values (alexandria:hash-table-values objects2)
;;             root)))


;; (defun migrate-objects-of-class (store class new-root-obj)
;;   (multiple-value-bind (objects root)
;;       (get-objects-of-class store class)
;;     (setf (gethash new-root-obj root)
;;           objects)
;;     (remhash class root)))


;; (defun migrate-store (store)
;;   (migrate-objects-of-class store 'HACRM.MODELS.CONTACT:CONTACT :contacts)
;;   (migrate-objects-of-class store 'HACRM.MODELS.FACTS.CORE:FACT :facts)
;;   (migrate-objects-of-class store 'HACRM.MODELS.FEED:FEED-ITEM :feed-items)
;;   (migrate-objects-of-class store 'HACRM.MODELS.NOTE:NOTE :notes)
;;   (migrate-objects-of-class store 'HACRM.MODELS.RELATION:RELATION :relations))
