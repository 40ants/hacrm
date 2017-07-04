(defpackage #:playground
  (:use #:cl))
(in-package #:playground)


(defstruct foo
  bar)


(defgeneric print-obj (obj))


(defmethod print-obj ((obj foo))
  (format t "Foo~%"))


(defmethod print-obj :around ((obj foo))
  (format t "Before~%")
  (call-next-method)
  (format t "After~%"))


(defmethod print-obj :after (obj)
  (format t "After ANY METHOD~%"))


(defclass object ()
  (;; (id :initform (progn (log:info "Making new id")
   ;;                      (random 100))
   ;;     :reader object-id)
   (name :initarg :name
         :initform "Bblah"
         :reader object-name)))

(defun make-object (name)
  (make-instance 'object :name name))

(defmethod object-id ((obj object))
  (log:info "Getting object id for" obj)
  (call-next-method))


(defmethod print-object ((obj object) stream)
  (format stream "Some object"))

(defmethod describe-object ((obj object) stream)
  (format stream "Some object description"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:bar
  (:use #:cl))
(in-package #:bar)


(defstruct (bar (:include playground::foo)))

(defmethod playground::print-obj :around ((obj bar))
  (format t "BAR Before~%")
  (call-next-method)
  (format t "BAR After~%"))


(defmethod playground::print-obj :before ((obj bar))
  (format t "Before BAR METHODs~%"))

(defmethod playground::print-obj :after ((obj bar))
  (format t "After BAR METHODs~%"))
