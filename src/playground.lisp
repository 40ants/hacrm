(defpackage #:playground
  (:use #:cl)
  (:import-from #:hacrm/models/contact
                #:make-contact)
  (:import-from #:hacrm/models/contact-utils
                #:find-contacts-by)
  (:import-from #:alexandria
                #:make-keyword
                #:with-gensyms)
  (:import-from #:hacrm/models/core
                #:get-root-object))
(in-package playground)


(defmacro fact-fields ((fact) &body body)
  `(list :|created-at| (hacrm/models/facts.core::created-at ,fact)
         :|updated-at| (hacrm/models/facts.core::updated-at ,fact)
         ,@body))

(defgeneric prepare-fact (fact)
  (:method ((fact hacrm/plugins/birthdays::birthday))
    (fact-fields (fact)
        :|type| "birthday"
        :|date| (hacrm/plugins/birthdays:date fact)))
  
  (:method ((fact hacrm/plugins/tags::tag))
    (fact-fields (fact)
        :|type| "tag"
        :|name| (hacrm/plugins/tags:name fact)))

  (:method ((fact hacrm/plugins/phone::phone))
    (fact-fields (fact)
        :|type| "phone"
        :|number| (hacrm/plugins/phone::get-number fact)))
  
  (:method ((fact hacrm/plugins/email::email))
    (fact-fields (fact)
        :|type| "email"
        :|address| (hacrm/plugins/email::address fact))))


(defun prepare-note (note)
  (list :|text| (hacrm/plugins/notes:text note)
        :|created-at| (hacrm/models/feed::created-at note)
        :|updated-at| (hacrm/models/feed::updated-at note)))


(defun export-data ()
  (let ((contacts
          (loop for contact in (hacrm/models/contact:all-contacts)
                for facts = (hacrm/models/facts/core:find-contact-facts contact)
                for notes = (hacrm/plugins/notes:get-contact-notes contact)
                collect (list
                         :|name| (hacrm/models/contact:name contact)
                         :|created| (hacrm/models/contact:created contact)
                         :|facts| (mapcar #'prepare-fact facts)
                         :|notes| (mapcar #'prepare-note notes)))))
    (jonathan:to-json contacts)))


(defun export-to-file (filename)
  (alexandria:with-output-to-file (stream filename)
    (write-string (export-data)
                  stream)))


(defun import-note (item)
  )


(defgeneric import-fact-of-type (type data)
  (:documentation "Imports a fact of given type. Type is a keyword, like :TAG or :BIRTHDAY"))


(defmacro with-json-fields ((&rest fields) form &rest body)
  "Works like with-slots, but for a plist dictionary, returned by jonathan:parse."
  (with-gensyms (data)
    (let* ((getters (loop for field in fields
                          for keyword = (make-keyword
                                         (string-downcase
                                          (symbol-name field)))
                          collect `(,field (getf ,data ,keyword)))))
      `(let* ((,data ,form)
              ,@getters)
         ,@body))))


(defmethod import-fact-of-type ((type (eql :phone)) data)
  (with-json-fields (number updated-at created-at) data
    (make-instance 'hacrm-phone/models::phone
                   :number number
                   :created-at created-at
                   :updated-at updated-at)))


(defmethod import-fact-of-type ((type (eql :birthday)) data)
  (with-json-fields (date updated-at created-at) data
    (make-instance 'hacrm-birthdays/models::birthday
                   :date date
                   :created-at created-at
                   :updated-at updated-at)))


(defmethod import-fact-of-type ((type (eql :email)) data)
  (with-json-fields (address updated-at created-at) data
    (make-instance 'hacrm-email/models::email
                   :address address
                   :created-at created-at
                   :updated-at updated-at)))


(defmethod import-fact-of-type ((type (eql :tag)) data)
  (with-json-fields (name updated-at created-at) data
    (make-instance 'hacrm-tags/models::tag
                   :name name
                   :created-at created-at
                   :updated-at updated-at)))


(defun import-fact (item)
  (let* ((type (getf item :|type|))
         (keyword-type (alexandria:make-keyword (string-upcase type))))
    (import-fact-of-type keyword-type
                         item)))


(defun import-note (data)
  (with-json-fields (text updated-at created-at) data
    (make-instance 'hacrm-notes/models::note
                   :text text
                   :created-at created-at
                   :updated-at updated-at)))


(defun import-contact (item)
  "Создаёт новый контакт из данных в JSON"
  (with-json-fields (name created facts notes) item
    (unless (find-contacts-by :name name)
      (hacrm/models/core:make-object
       'hacrm/models/contact::contact
       :name name
       :created-at created
       :facts (mapcar #'import-fact facts)
       :feed-items (mapcar #'import-note notes)))))


(defun import-from-file (filename)
  (let* ((content (alexandria:read-file-into-string filename))
         (items (jonathan:parse content)))
    (loop for item in items
                 for contact = (import-contact item)
                 when contact
                   do (push contact
                            (get-root-object :contacts)))))
