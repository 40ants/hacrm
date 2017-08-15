(in-package :hacrm)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.

(defvar *default-db-path*
  #P "~/.hacrm/db/"
  "This path is used for production data.")

(defvar *dev-db-path*
  #P"~/.hacrm/dev-db/"
  "This path is used for development"
)


(defclass hacrm-prevalence-system (cl-prevalence:guarded-prevalence-system)
  ())

(asdf:defsystem weblocks-hacrm-prevalence
  :name "weblocks-hacrm-prevalence"
  :version "0.0.1"
  :maintainer ""
  :author ""
  :licence ""
  :description "Fake system to make weblocks-stores happy"
  :depends-on ())


(defmethod weblocks-stores:open-store ((store-type (eql :hacrm-prevalence)) &rest args)
  (let* ((store (apply #'make-instance 'hacrm-prevalence-system :directory (car args) (cdr args)))
         (lock-name (format nil "Prevalence lock for store ~S" store))
         (lock (bordeaux-threads:make-lock lock-name)))
    (setf (gethash store weblocks-prevalence::*locks*) lock)
    (setf (weblocks-prevalence::get-guard store)
          (lambda (thunk)
            (bordeaux-threads:with-lock-held (lock)
              (funcall thunk))))
    (setf weblocks-stores:*default-store* store)))


(weblocks-stores:defstore *hacrm-store* :hacrm-prevalence *default-db-path*)

