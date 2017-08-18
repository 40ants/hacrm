(in-package hacrm.plugins.email)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :add-email))
                                   address)
  "Add an email to the contact."
  
  (let* ((contact (hacrm.widgets.contact-details:get-contact widget)))
    (log:debug "Adding email" contact address)
  

    (let ((email-fact (add-email contact address)))

      (hacrm.widgets.main:reset-user-input widget)
      (weblocks:mark-dirty widget)
      (weblocks.hooks:eval-hooks :fact-created
                                 contact
                                 email-fact)
      (values))))


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :remove-email))
                                   address)
  "Remove email from the contact."
  
  (let* ((contact (hacrm.widgets.contact-details:get-contact widget)))
    (log:debug "Removing email" contact address)

    (let ((removed-facts (remove-email contact address)))

      (when removed-facts
        (hacrm.widgets.main:reset-user-input widget)
        (weblocks:mark-dirty widget)
        
        (dolist (fact removed-facts)
          (weblocks.hooks:eval-hooks :fact-removed
                                     contact
                                     fact))
        (values)))))

