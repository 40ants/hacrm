(in-package hacrm.plugins.notes)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details)
                                   (keyword (eql :note))
                                   rest-text)
  (declare (ignorable keyword))

  (log:info "Adding a note for the contact")
  
  (let* ((contact (hacrm.widgets.contact-details:get-contact widget))
         (note (add-note contact rest-text)))

    (hacrm.widgets.main:reset-user-input widget)
    
    (weblocks/hooks:with-feed-item-created-hook (note))
    (values)))
