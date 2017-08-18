(defpackage #:hacrm.plugins.email
  (:use #:cl
        #:hacrm.models.facts.core
        #:f-underscore)
  (:export
   #:get-emails
   #:add-email
   #:remove-email
   #:address))
