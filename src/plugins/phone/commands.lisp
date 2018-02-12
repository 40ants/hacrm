(defpackage #:hacrm/plugins/phone/commands
  (:use #:cl))
(in-package hacrm/plugins/phone/commands)


(defmethod hacrm/commands:command ((widget hacrm/widgets/contact-details:contact-details)
                                   (keyword (eql :add-phone))
                                   number)
  "Add an phone to the contact."
  
  (let* ((contact (hacrm/widgets/contact-details:get-contact widget)))
    (log:debug "Adding phone" contact number)
  

    (let ((phone-fact (add-phone contact number)))

      (hacrm/widgets/main:reset-user-input widget)
      (weblocks/widget:update widget)
      (weblocks/hooks:call-fact-created-hook contact phone-fact)
      (values))))


(defmethod hacrm/commands:command ((widget hacrm/widgets/contact-details:contact-details)
                                   (keyword (eql :remove-phone))
                                   number)
  "Remove phone from the contact."
  
  (let* ((contact (hacrm/widgets/contact-details:get-contact widget)))
    (log:debug "Removing phone" contact number)

    (let ((removed-facts (remove-phone contact number)))

      (when removed-facts
        (hacrm/widgets/main:reset-user-input widget)
        (weblocks/widget:update widget)
        
        (dolist (fact removed-facts)
          (weblocks/hooks:call-fact-removed-hook contact fact))
        (values)))))

