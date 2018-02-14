(defpackage #:hacrm-birthdays/commands
  (:use #:cl)
  (:import-from #:hacrm/widgets/contact-details
                #:get-contact
                #:contact-details)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/main
                #:reset-user-input)
  (:import-from #:weblocks/widget
                #:update)
  (:import-from #:weblocks/hooks
                #:call-fact-created-hook)
  (:import-from #:hacrm-birthdays/models
                #:set-birthday))
(in-package hacrm-birthdays/commands)


(defmethod command ((widget contact-details)
                    (keyword (eql :birthday))
                    date)
  "Sets a birthday. Accepts one argument like 1980-03-15"
  
  (let* ((contact (get-contact widget)))
    (log:debug "Setting a birthday" contact date)
  

    (let ((birthday (set-birthday contact date)))

      (reset-user-input widget)
      (update widget)
      (call-fact-created-hook contact birthday)
      (values))))

