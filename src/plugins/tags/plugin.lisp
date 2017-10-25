(defpackage #:hacrm.plugins.tags
  (:use #:cl
        #:cl-who
        #:hacrm.models.core
        #:hacrm.models.facts.core
        #:f-underscore)
  (:import-from #:hacrm.models.facts.core
                #:contact)
  (:export
   #:tag-contact
   #:name
   #:contact
   #:tag
   #:get-contact-tags
   #:tag-contact
   #:untag-contact))
