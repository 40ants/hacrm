(defpackage #:hacrm.models.relation
  (:use #:cl
        #:weblocks
        #:f-underscore)
  (:export
   #:make-relation
   #:relation
   #:left
   #:right
   #:created-at
   #:type
   #:find-relation-from-object))
(in-package hacrm.models.relation)


(defclass relation ()
  ((id)
   (left :initarg :left
         :reader left)
   (right :initarg :right
          :reader right)
   (type :type keyword
         :initarg :type
         :reader type)
   (created-at :type integer
               :initform (get-universal-time)
               :reader created-at)))


(defun make-relation (type left right)
  (make-instance 'relation
                 :type type
                 :left left
                 :right right))


(defun find-relation-from-object (obj &key type)
  "Returns relations where given object is in the `left' part,
optionally filtering by relation's type."
  (hacrm.utils:find-object
   'relation
   :filter (lambda (relation)
             (and (eql (weblocks-stores:object-id (left relation))
                       (weblocks-stores:object-id obj))
                  (or (null type)
                      (eql type
                           (type relation)))))))
