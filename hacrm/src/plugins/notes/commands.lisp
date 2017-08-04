(in-package hacrm.plugins.notes)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :note))
                                   rest-text)
  (declare (ignorable keyword))

  (log:debug "Adding a note for the contact")
  
  (let* ((contact (hacrm.widgets.contact-details:get-contact widget))
         (note (make-note rest-text))
         (relation (hacrm.models.relation:make-relation
                    :activity contact note)))
    (hacrm.utils:store-object note)
    (hacrm.utils:store-object relation))

  (hacrm.widgets.main:reset-user-input widget)
  (mark-dirty widget))
