
(in-package :hacrm)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *hacrm-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :hacrm)))

