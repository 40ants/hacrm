(defpackage #:hacrm/plugins/email/commands
  (:use #:cl))
(in-package hacrm/plugins/email/commands)


(defmethod hacrm/commands:command ((widget hacrm/widgets/contact-details:contact-details)
                                   (keyword (eql :add-email))
                                   address)
  "Add an email to the contact."
  
  (let* ((contact (hacrm/widgets/contact-details:get-contact widget)))
    (log:debug "Adding email" contact address)
  

    (let ((email-fact (add-email contact address)))

      (hacrm/widgets/main:reset-user-input widget)
      (weblocks/widget:update widget)
      (weblocks/hooks:call-fact-created-hook contact email-fact)
      (values))))


(defmethod hacrm/commands:command ((widget hacrm/widgets/contact-details:contact-details)
                                   (keyword (eql :remove-email))
                                   address)
  "Remove email from the contact."
  
  (let* ((contact (hacrm/widgets/contact-details:get-contact widget)))
    (log:debug "Removing email" contact address)

    (let ((removed-facts (remove-email contact address)))

      (when removed-facts
        (hacrm/widgets/main:reset-user-input widget)
        (weblocks/widget:update widget)
        
        (dolist (fact removed-facts)
          (weblocks/hooks:call-fact-removed-hook contact fact))
        (values)))))

