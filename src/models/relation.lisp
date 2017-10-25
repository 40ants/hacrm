(defpackage #:hacrm.models.relation
  (:use #:cl
        #:weblocks
        #:f-underscore)
  (:export
   #:make-relation
   #:relation
   #:left
   #:right
   #:created-at
   #:type
   #:find-relation-from-object))
(in-package hacrm.models.relation)


(defclass relation (hacrm.models.core:base)
  ((left :initarg :left
         :reader left)
   (right :initarg :right
          :reader right)
   (type :type keyword
         :initarg :type
         :reader type)
   (created-at :type integer
               :initform (get-universal-time)
               :reader created-at)))


(defmethod print-object ((obj relation) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a <-> ~a"
            (left obj)
            (right obj))))

(defun make-relation (type left right)
  (make-instance 'relation
                 :type type
                 :left left
                 :right right))


(defun find-relation-from-object (obj &key type)
  "Returns relations where given object is in the `left' part,
optionally filtering by relation's type."
  (hacrm.utils:find-object
   :relations
   :filter (lambda (relation)
             (and (equal (left relation)
                         obj)
                  (or (null type)
                      (eql type
                           (type relation)))))))


(defun migrate-relations ()
  "Временная функция чтобы трансформировать данные."
  (let* ((all (hacrm.models.core:get-root-object :relations))
         (contacts (hacrm.models.core:get-root-object :contacts))
         ;; Так как из-а weblocks-stores появились дубли объектов, то приходится матчить по имени
         (contacts-by-name (make-hash-table :test 'equal))
         (notes (hacrm.models.core:get-root-object :feed-items))
         (notes-by-text (make-hash-table :test 'equal)))

    ;; Заполним словарик с контактами
    (loop for contact in contacts
          do (setf (gethash (hacrm.models.contact:name contact)
                            contacts-by-name)
                   contact))

    ;; Заполним словарик с заметками
    (loop for note in notes
          do (setf (gethash (hacrm.plugins.notes:text note)
                            notes-by-text)
                   note))

    (log:info "Transforming relations: " (length all))
    (loop for relation in all
          do (let* ((note-text (hacrm.plugins.notes:text (right relation)))
                    (note (gethash note-text
                                   notes-by-text))
                    (contact-name (hacrm.models.contact:name (left relation)))
                    (contact (gethash contact-name
                                      contacts-by-name)))
               (setf (hacrm.plugins.notes:get-object note)
                     contact)))))


