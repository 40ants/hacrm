(defpackage #:hacrm/models/note
  (:use #:cl #:weblocks #:f-underscore)
  (:import-from #:prevalence-multimaster/system
                #:get-root-object)
  (:export #:note
           #:make-note
           #:save-note
           #:note-text
           #:note-created
           #:note-contact
           #:find-notes
           #:for-each
           #:created
           #:text
           #:contact))
(in-package hacrm/models/note)


(defclass note (hacrm/models/core:base)
  ((contact :initarg :contact
            :reader note-contact)
   (text :type string
         :initarg :text
         :accessor note-text)
   (created :type integer
            :initform (get-universal-time)
            :reader note-created)))

(defun make-note (contact text)
  (make-instance 'note
                 :contact contact
                 :text text))


(defun save-note (note)
  (error "Removed")
  ;; (weblocks-stores:persist-object
  ;;  hacrm::*store*
  ;;  note)
  )


(defun find-notes (contact &key (order-by '(created . :desc)))
  (error "Removed")
  ;; (weblocks-stores:find-persistent-objects
  ;;  hacrm::*store*
  ;;  'note
  ;;  :filter (f_ (equal (note-contact _)
  ;;                     contact))
  ;;  :order-by order-by)
  )


(defmethod print-object ((note note) stream)
  (format stream "#<NOTE created=~A text=~S>"
          (hacrm/utils:format-time (note-created note))
          (hacrm/utils:first-line (note-text note))))


(defmacro for-each (var-name &body body)
  `(error "For-each was removed")
  ;; (loop for ,var-name in (weblocks-stores:find-persistent-objects
  ;;                          hacrm::*store*
  ;;                          'hacrm/models/note:note)
  ;;        do ,@body)
  )


(defun migrate-old-notes ()
  "Временная функция чтобы трансформировать данные перенеся данные на плагин"
  
  (let* ((old-notes (get-root-object :notes))
         (contacts (get-root-object :contacts))
         (contacts-by-name (make-hash-table :test 'equal)))

    ;; Заполним словарик с контактами
    (loop for contact in contacts
          do (setf (gethash (hacrm/models/contact:name contact)
                            contacts-by-name)
                   contact))

    (loop for note in old-notes
          collect (let* ((note-text (note-text note))
                         (contact-name (hacrm/models/contact:name (note-contact note)))
                         (contact (gethash (cl-strings:clean contact-name)
                                           contacts-by-name)))

                    (hacrm/plugins/notes:add-note contact note-text))))) 
