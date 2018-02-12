(defpackage #:hacrm/models/playground
  (:use #:cl #:f-underscore))
(in-package hacrm/models/playground)


;; (defclass fact ()
;;   ((id)))


;; (defmacro deffact (name slots)
;;   `(prog1
;;        (defclass ,name (fact)
;;          ,slots)
     
;;      (defmethod cl-prevalence::get-root-object ((system hacrm::hacrm-prevalence-system)
;;                                                 (name (eql ',name)))
;;        (cl-prevalence::get-root-object system 'hacrm.models.playground::fact))


;;      (defmethod (setf cl-prevalence::get-root-object) (value
;;                                                        (system hacrm::hacrm-prevalence-system)
;;                                                        (name (eql ',name)))
;;        (setf (cl-prevalence::get-root-object system
;;                                              'hacrm.models.playground::fact)
;;              value))))


;; (deffact skype
;;     ((nickname :type string
;;                :initarg :nickname
;;                :accessor get-nickname)))

;; (defmethod print-object ((fact skype) stream)
;;   (format stream "#<TWITTER ~a>" (get-nickname fact)))


;; (defclass twitter (fact)
;;   ((nickname :type string
;;              :initarg :nickname
;;              :accessor get-nickname)))


;; (defmethod print-object ((fact twitter) stream)
;;   (format stream "#<TWITTER ~a>" (get-nickname fact)))


;; (defclass email (fact)
;;   ((email :type string
;;           :initarg :email
;;           :accessor get-email)))


;; (defmethod print-object ((fact email) stream)
;;   (format stream "#<EMAIL ~a>" (get-email fact)))


(defun save-object (obj)
  (weblocks-stores:persist-object
   hacrm::*store*
   obj))


(defun find-facts ()
  (weblocks-stores:find-persistent-objects
   hacrm::*store*
   'fact))
