(defpackage #:playground
  (:use #:cl))
(in-package playground)


(defmacro fact-fields ((fact) &body body)
  `(list :|created-at| (hacrm.models.facts.core::created-at ,fact)
         :|updated-at| (hacrm.models.facts.core::updated-at ,fact)
         ,@body))

(defgeneric prepare-fact (fact)
  (:method ((fact hacrm.plugins.birthdays::birthday))
    (fact-fields (fact)
        :|type| "birthday"
        :|date| (hacrm.plugins.birthdays:date fact)))
  
  (:method ((fact hacrm.plugins.tags::tag))
    (fact-fields (fact)
        :|type| "tag"
        :|name| (hacrm.plugins.tags:name fact)))

  (:method ((fact hacrm.plugins.phone::phone))
    (fact-fields (fact)
        :|type| "phone"
        :|number| (hacrm.plugins.phone::get-number fact)))
  
  (:method ((fact hacrm.plugins.email::email))
    (fact-fields (fact)
        :|type| "email"
        :|address| (hacrm.plugins.email::address fact))))


(defun prepare-note (note)
  (list :|text| (hacrm.plugins.notes:text note)
        :|created-at| (hacrm.models.feed::created-at note)
        :|updated-at| (hacrm.models.feed::updated-at note)))


(defun export-data ()
  (let ((contacts
          (loop for contact in (hacrm.models.contact:all-contacts)
                for facts = (hacrm.models.facts.core:find-contact-facts contact)
                for notes = (hacrm.plugins.notes:get-contact-notes contact)
                collect (list
                         :|name| (hacrm.models.contact:name contact)
                         :|created| (hacrm.models.contact:created contact)
                         :|facts| (mapcar #'prepare-fact facts)
                         :|notes| (mapcar #'prepare-note notes)))))
    (jonathan:to-json contacts)))


(defun export-to-file (filename)
  (alexandria:with-output-to-file (stream filename)
    (write-string (export-data)
                  stream)))
