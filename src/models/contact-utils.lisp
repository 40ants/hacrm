(defpackage #:hacrm/models/contact-utils
  (:use #:cl
        #:f-underscore)
  (:documentation "We need to separate these functions to break curcular dependency between facts/core and models/contact.")
  (:import-from #:hacrm/models/core
                #:get-object-id
                #:find-object)
  (:import-from #:alexandria
                #:with-gensyms)
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


(defmacro add-list-items (slot-name (contact-id) &rest items)
  "Adds given items to the contact's slot."
  (with-gensyms (contact)
    `(let* ((,contact (find-contact-by :id ,contact-id))
            (new-items (list ,@items)))

       (unless ,contact
         (error "Contact with id ~A not found" ,contact-id))
       
       (dolist (item new-items)
         (push item
               (slot-value ,contact
                           ',slot-name)))

       (values new-items))))


(defmacro remove-list-items (slot-name item-name (contact-id &key type) &body rules)
  "Removes a items from the list accoring to rules.

A rules is an expression, evaluated to check if the item should be removed.
For example:

\(remove-facts \(contact-id :type 'tag\)
     \(string-equal \(get-number item\)
                     phone-number\)\)

Will remove all phone numbers where number is equal to given.

Variables 'contact' and 'item' are bound to a contact identified by contact-id,
and to checked item during rules evaluation.

Returns a list of removed facts.
"
  (with-gensyms (contact all-items filtered-items removed-items)
    `(let* ((,contact (find-contact-by :id ,contact-id))
            ;; Tags are the facts which stored in a facts collection
            (,all-items (slot-value ,contact
                                    ',slot-name))
            ;; Now we need to filter-out facts, related to the contact and
            ;; having the given name
            ,filtered-items
            ,removed-items)
       
       (dolist (,item-name ,all-items)
         ;; TODO: it is possible to move
         ;; this rules construction to the macro level
         ;; to optimize performance
         (if (and (or (null ,type)
                      (typep ,item-name ,type))
                  ,@rules)
             (push ,item-name ,removed-items)
             (push ,item-name ,filtered-items)))
       ;; Now, filtered items should be saved back to the collection
       (setf (slot-value ,contact ',slot-name)
             (nreverse ,filtered-items))

       ;; Return removed items
       (nreverse ,removed-items))))
