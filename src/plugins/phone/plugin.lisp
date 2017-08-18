(defpackage #:hacrm.plugins.phone
  (:use #:cl
        #:hacrm.models.facts.core
        #:f-underscore)
  (:export
   #:get-phones
   #:add-phone
   #:remove-phone
   #:number))
