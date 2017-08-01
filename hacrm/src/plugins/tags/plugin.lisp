(defpackage #:hacrm.plugins.tags
  (:use #:cl
        #:cl-who
        #:hacrm.models.facts.core
        #:f-underscore)
  (:export
   #:make-tag-fact
   #:name
   #:tag
   #:get-contact-tags))
