(defpackage #:hacrm/plugins/phone/plugin
  (:nicknames #:hacrm/plugins/phone)
  (:use #:cl
        ;; #:hacrm.models.core
        ;; #:hacrm.models.facts.core
        ;; #:f-underscore
        )
  (:export
   #:get-phones
   #:add-phone
   #:remove-phone
   #:number))
