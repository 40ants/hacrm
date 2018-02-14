(defpackage #:hacrm-phone/commands
  (:use #:cl)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/contact-details
                #:get-contact
                #:contact-details)
  (:import-from #:hacrm-phone/models
                #:remove-phone
                #:add-phone)
  (:import-from #:hacrm/widgets/main
                #:reset-user-input)
  (:import-from #:weblocks/widget
                #:update)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook))
(in-package hacrm-phone/commands)


(defmethod command ((widget contact-details)
                    (keyword (eql :add-phone))
                    number)
  "Add an phone to the contact."
  
  (let* ((contact (get-contact widget)))
    (log:debug "Adding phone" contact number)
  

    (let ((phone-fact (add-phone contact number)))

      (reset-user-input widget)
      (update widget)
      (call-fact-created-hook contact phone-fact)
      (values))))


(defmethod command ((widget contact-details)
                    (keyword (eql :remove-phone))
                    number)
  "Remove phone from the contact."
  
  (let* ((contact (get-contact widget)))
    (log:debug "Removing phone" contact number)

    (let ((removed-facts (remove-phone contact number)))

      (when removed-facts
        (reset-user-input widget)
        (update widget)
        
        (dolist (fact removed-facts)
          (call-fact-removed-hook contact fact))
        (values)))))

