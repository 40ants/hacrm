(defpackage #:hacrm.plugins.email
  (:use #:cl
        #:hacrm.models.core
        #:hacrm.models.facts.core
        #:f-underscore)
  (:export
   #:get-emails
   #:add-email
   #:remove-email
   #:address
   #:already-exists
   #:email
   #:other-contact))
