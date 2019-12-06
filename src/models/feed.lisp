(defpackage #:hacrm/models/feed
  (:use #:cl
        #:f-underscore)
  ;; (:nicknames #:HACRM.MODELS.FEED)
  (:import-from #:hacrm/models/contact-utils
                #:add-list-items
                #:remove-list-items)
  (:export
   #:def-feed-item
   #:feed-item
   #:get-feed-items
   #:object-with-feed-mixin
   #:remove-feed-items
   #:add-feed-items
   #:get-created-at
   #:get-updated-at))
(in-package hacrm/models/feed)


(defclass feed-item (hacrm/models/core:base)
  ((created-at :initform (get-universal-time)
               :initarg :created-at
               :reader get-created-at)
   (updated-at :initform (get-universal-time)
               :initarg :updated-at
               :reader get-updated-at)))


(defmacro def-feed-item (name slots)
  "This macro defines a new class, based on `feed-item' and setup
a methods to store items of this class in a collection with items
of other types."
  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (feed-item)
         ,slots)))


(defclass object-with-feed-mixin ()
  ((feed-items :type list
               :initform nil
               :initarg :feed-items
               :documentation "A list of feed items, associated with object. Contains objects of classes, derived from `feed-item'.")))


(defgeneric get-feed-items (object)
  (:documentation "Returns a feed items, associated with the object."))


(defmethod get-feed-items ((object t))
  "By default, we return an empty list, because no item is associated with the object."
  nil)


(defmethod get-feed-items ((object object-with-feed-mixin))
  "By default, we return an empty list, because no item is associated with the object."
  (slot-value object
              'feed-items))


(defmacro add-feed-items ((contact-id) &body items)
  `(add-list-items feed-items
                   (,contact-id)
                   ,@items))


(defmacro remove-feed-items ((item-name contact-id &key type) &body rules)
  `(remove-list-items
       feed-items
       ,item-name
       (,contact-id :type ,type)
       ,@rules))
