(defpackage #:playground
  (:use #:cl))
(in-package playground)


(defparameter *system-location* (pathname "/tmp/test-prevalence-system/"))


(defclass node ()
  ((id :initform (random 100000000)
       :reader get-id)
   (name :initarg :name
         :reader get-name)))


(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "name=~a" (get-name obj))))


(defclass edge ()
  ((from :initarg :from
         :reader get-from)
   (to :initarg :to
       :reader get-to)
   (name :initarg :name
         :reader get-name)))


(defun tx-init-system (system)
  (setf (cl-prevalence:get-root-object system :nodes)
        nil)
  (setf (cl-prevalence:get-root-object system :edges)
        nil))


(defun init-system (system)
  (unless (cl-prevalence:get-root-object system :nodes)
    (cl-prevalence:execute system (cl-prevalence:make-transaction 'tx-init-system)))
  system)


(defvar *db* (init-system (cl-prevalence:make-prevalence-system *system-location*)))


(defun tx-create-node (system name)
  (push
   (make-instance 'node :name name)
   (cl-prevalence:get-root-object system :nodes)))


(defun create-node (name)
  (cl-prevalence:execute *db*
                         (cl-prevalence:make-transaction
                          'tx-create-node
                          name)))

(defun tx-create-edge (system name from to)
  (push (make-instance 'edge
                       :name name
                       :from from
                       :to to)
        (cl-prevalence:get-root-object system :edges)))


(defun create-edge (name from to)
  (cl-prevalence:execute *db*
                         (cl-prevalence:make-transaction
                          'tx-create-edge
                          name
                          from
                          to)))
