(defpackage #:hacrm-email/commands
  (:use #:cl)
  (:import-from #:hacrm/commands
                #:command)
  (:import-from #:hacrm/widgets/contact-details
                #:get-contact
                #:contact-details)
  (:import-from #:hacrm-email/models
                #:remove-email
                #:add-email)
  (:import-from #:hacrm/widgets/main
                #:reset-user-input)
  (:import-from #:weblocks/widget
                #:update)
  (:import-from #:weblocks/hooks
                #:call-fact-removed-hook
                #:call-fact-created-hook))
(in-package hacrm-email/commands)


(defmethod command ((widget contact-details)
                    (keyword (eql :add-email))
                    address)
  "Add an email to the contact."
  
  (let* ((contact (get-contact widget)))
    (log:debug "Adding email" contact address)
  

    (let ((email-fact (add-email contact address)))

      (reset-user-input widget)
      (update widget)
      (call-fact-created-hook contact email-fact)
      (values))))


(defmethod command ((widget contact-details)
                    (keyword (eql :remove-email))
                                   address)
  "Remove email from the contact."
  
  (let* ((contact (get-contact widget)))
    (log:debug "Removing email" contact address)

    (let ((removed-facts (remove-email contact address)))

      (when removed-facts
        (reset-user-input widget)
        (update widget)
        
        (dolist (fact removed-facts)
          (call-fact-removed-hook contact fact))
        (values)))))

