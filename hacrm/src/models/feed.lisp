(defpackage #:hacrm.models.feed
  (:use #:cl
        #:f-underscore)
  (:export
   #:def-feed-item
   #:feed-item
   #:created-at
   #:updated-at))
(in-package hacrm.models.feed)


(defclass feed-item ()
  ((id)
   (created-at :initform (get-universal-time)
               :reader created-at)
   (updated-at :initform (get-universal-time)
               :reader updated-at)))


(defmacro def-feed-item (name slots)
  "This macro defines a new class, based on `feed-item' and setup
a methods to store items of this class in a collection with items
of other types."
  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name (feed-item)
         ,slots)
     
     (defmethod cl-prevalence::get-root-object ((system hacrm::hacrm-prevalence-system)
                                                (name (eql ',name)))
       (cl-prevalence::get-root-object system 'feed-item))


     (defmethod (setf cl-prevalence::get-root-object) (value
                                                       (system hacrm::hacrm-prevalence-system)
                                                       (name (eql ',name)))
       (setf (cl-prevalence::get-root-object system
                                             'feed-item)
             value))

     (defmethod weblocks-stores:find-persistent-objects :around
         ((store hacrm::hacrm-prevalence-system)
          (class-name (eql ',name)) 
          &key (filter nil)
            order-by
            range
            slot
            (value nil value-given-p)
            (test #'equal))
       "A wrapper to filter feed items by they actual type."

       (log:debug "Searching for obj of" class-name)
       ;; TODO: unit-test it
       (let* ((filter (if filter
                          (f_ (and (typep _ ',name)
                                   (funcall filter _)))
                          (f_ (typep _ ',name))))
              (params (append (list store
                                    class-name
                                    :filter filter
                                    :order-by order-by
                                    :range range
                                    :slot slot
                                    :test test)
                              (when value-given-p
                                (list :value value)))))
         (apply #'call-next-method params)))))

