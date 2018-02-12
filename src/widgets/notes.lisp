(defpackage #:hacrm/widgets/notes
  (:use #:cl
        #:hacrm)
  (:export
   :render-contact-notes
   :make-notes-widget
   :notes-contact))
(in-package hacrm/widgets/notes)


(weblocks/widget:defwidget notes ()
  ((contact :initform nil
            :initarg :contact
            :accessor notes-contact)))


(defmethod (setf notes-contact) :after (contact (widget notes))
  (declare (ignorable contact))
  (weblocks/widget:update widget))


(defun make-notes-widget ()
  (make-instance 'notes))


(defun add-new-note (widget contact &key text &allow-other-keys)
  (log:debug "Adding new note for" contact "with" text)
  ;; (hacrm/models/note:save-note 
  ;;  (hacrm/models/note:make-note contact text))
  (weblocks/widget:update widget))


(defun render-new-note-form (widget contact)
  (declare (type hacrm/models/contact:contact contact))
  
  (with-html-form (:post (lambda (&rest args)
                           (apply #'add-new-note
                                  widget
                                  contact
                                  args)))
    (:h1 "Добавить новую заметку")
    
    (:p (:label :for "text"
                "Текст заметки")
        (:textarea :name "text"
                   :value ""))
    (:p (:input :type "submit"
                :class "btn btn-primary"
                :value "Добавить"))))


(defun render (note)
  ;; (with-accessors ((created hacrm/models/note:note-created)
  ;;                  (text hacrm/models/note:note-text)) note
  ;;   (with-html (:li (:p "Дата: " (esc (hacrm/utils:format-time
  ;;                                      created)))
  ;;                   (:p (str (hacrm/utils:text->html
  ;;                             text))))))
  )


(defmethod weblocks/widget:render ((widget notes))
  (let* ((contact (notes-contact widget))
         (notes ;; (hacrm/models.note:find-notes contact)
                ))
    (when contact
      (render-new-note-form widget contact))
    
    (weblocks/html:with-html
      (:h1 "Заметки")
      (:ul (loop for note in notes
                 do (render note))))))
