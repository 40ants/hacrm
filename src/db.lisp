(defpackage #:hacrm/db
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:weblocks/hooks)
  (:import-from #:cl-prevalence
                #:get-transaction-hook
                #:get-guard
                #:guarded-prevalence-system)
  (:import-from #:hacrm/utils
                #:get-machine-name)
  (:import-from #:bordeaux-threads
                #:thread-alive-p
                #:destroy-thread
                #:make-thread)
  (:import-from #:prevalence-multimaster/sync
                #:sync-with-other-masters)
  (:import-from #:osicat
                #:delete-directory-and-files)
  (:export
   #:make-snapshot))
(in-package hacrm/db)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.

(defvar *store* nil
  "Main database for all items operated by CRM.")


(defparameter *default-db-path*
  ;; #P "~/.hacrm/db/"
  "~/Dropbox/hacrm"
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


;; (defclass hacrm-prevalence-system (guarded-prevalence-system)
;;   ())

(defvar *sync-thread* nil)

(defparameter *sleep-between-syncs* 15
  "A number of seconds before doing snapshots for syncronization.")


(defun synchronize ()
  (loop
    do (ignore-errors
        (log4cl-json:with-log-unhandled ()
          (log:info "Syncronizing with other masters")
          (when *store*
            (sync-with-other-masters *store*))))
       (sleep *sleep-between-syncs*)))


(defun start-sync-thread ()
  (when (and *sync-thread*
             (thread-alive-p *sync-thread*))
    (destroy-thread *sync-thread*))
  
  (setf *sync-thread*
        (make-thread 'synchronize
                     :name "syncronizer"))
  (values))


(defun open-store (&optional (path *default-db-path*)
                             (machine-name (get-machine-name))
                   &rest args)
  (declare (ignorable args))
  
  (when *store*
    (error "Please, close previously opened store before opening the new one."))
  
  (let* ((store (prevalence-multimaster/system:make-system path machine-name))
         (lock-name (format nil "Prevalence lock for store ~S" store))
         (lock (bordeaux-threads:make-recursive-lock lock-name)))
    (setf (get-guard store)
          (lambda (thunk)
            (bordeaux-threads:with-recursive-lock-held (lock)
              (funcall thunk))))

    (setf (get-transaction-hook store)
          (lambda (transaction)
            (log:info "Executing transaction" transaction)
            (incf *transaction-log-length*)
            
            (when *log-transactions*
              (push transaction
                    *transactions*))))

    (setf *store* store)
    (start-sync-thread)
    store))


(defun make-snapshot ()
  (when *store*
    (cl-prevalence:snapshot *store*)))


(defun close-store ()
  (when *store*
    (make-snapshot)
    (setf *store* nil)))


(defun reopen-store ()
  (close-store)
  (open-store))


(defun reset-store ()
  (let ((directory (when *store*
                     (cl-prevalence::get-directory *store*))))
    (close-store)
    (when directory
      (delete-directory-and-files directory)))
  (open-store))


(defun switch-to-db (path)
  (close-store)
  (open-store path))


(defun dev-db ()
  (switch-to-db *dev-db-path*))


(defun default-db ()
  (switch-to-db *default-db-path*))


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

(weblocks/hooks:on-application-hook-handle-http-request
  setup-prevalence-system ()
  (prevalence-multimaster/system:with-system (*store*)
    (weblocks/hooks:call-next-hook)))
