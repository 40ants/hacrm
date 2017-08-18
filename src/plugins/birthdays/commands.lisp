(in-package hacrm.plugins.birthdays)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :birthday))
                                   date)
  "Sets a birthday. Accepts one argument like 1980-03-15"
  
  (let* ((contact (hacrm.widgets.contact-details:get-contact widget)))
    (log:debug "Setting a birthday" contact date)
  

    (let ((birthday (set-birthday contact date)))

      (hacrm.widgets.main:reset-user-input widget)
      (weblocks:mark-dirty widget)
      (weblocks.hooks:eval-hooks :fact-created
                                 contact
                                 birthday)
      (values))))
