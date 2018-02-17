(defpackage #:hacrm/models/contact-utils
  (:use #:cl
        #:f-underscore)
  (:documentation "We need to separate these functions to break curcular dependency between facts/core and models/contact.")
  (:import-from #:hacrm/models/core
                #:get-object-id
                #:find-object)
  (:export
   #:find-contact-by
   #:find-contacts-by))
(in-package hacrm/models/contact-utils)


(defgeneric find-contacts-by (keyword value)
  (:documentation "Searches contacts by different facts, for example, by email, or twitter nickname.

Plugins should define methods for this generic, if they want to give ability to search
contacts by some associated data."))


(defun find-contact-by (keyword value)
  (let ((results (find-contacts-by keyword value)))
    (when (> (length results) 1)
      (error "More then one contact was found by ~A = ~A" keyword value))
    (first results)))
