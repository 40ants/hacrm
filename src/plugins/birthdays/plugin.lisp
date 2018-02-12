(defpackage #:hacrm/plugins/birthdays/plugin
  (:nicknames #:hacrm/plugins/birthdays)
  (:use #:cl)
  (:export
   #:make-birthday-fact
   #:date
   #:invalid-date-format
   #:get-birthday
   #:set-birthday))
(in-package hacrm/plugins/birthdays/plugin)

;; (defpackage #:hacrm.plugins.birthdays
;;   (:use #:cl
;;         #:hacrm.models.core
;;         #:hacrm.models.facts.core
;;         #:f-underscore)
;; )
