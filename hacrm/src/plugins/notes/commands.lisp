(in-package hacrm.plugins.notes)


(defmethod hacrm.commands:command ((widget hacrm.widgets.contact-details:contact-details2)
                                   (keyword (eql :note))
                                   rest-text)
  (declare (ignorable keyword))

  (log:debug "Adding a note for the contact")
  
  (let* ((note (make-note rest-text)))
    (hacrm.utils:store-object note))

  (hacrm.widgets.main:reset-user-input widget)
  (mark-dirty widget))
