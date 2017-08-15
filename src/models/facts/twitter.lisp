(defpackage #:hacrm.models.facts.twitter
  (:use #:cl
        #:hacrm.models.facts.core)
  (:export
   #:make-twitter-fact))
(in-package hacrm.models.facts.twitter)


(deffact twitter
  ((nickname :type string
             :initarg :nickname
             :accessor nickname)))


(defmethod print-object ((fact twitter) stream)
  (print-unreadable-object (fact stream :type t :identity t)
    (format stream "~a" (nickname fact))))


(defun make-twitter-fact (contact nickname)
  (make-instance 'twitter
                 :contact contact
                 :nickname nickname))
