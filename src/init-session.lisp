(in-package hacrm)


;; (defwidget list-dependencies ()
;;   ())


;; (defmethod render-widget-body ((widget list-dependencies) &rest args)
;;   (declare (ignorable args))
;;   (with-html
;;     (:ul :class "page-dependencies"
;;          (loop :for item :in (append (webapp-application-dependencies)
;;                                      weblocks::*page-dependencies*)
;;                :do (htm
;;                     (:li (esc (format nil "dep: ~s" item))))
;;                )
;;          )))


(defun get-debug-style ()
  (lass:compile-and-write
   '(body
     ((:and .debug-frame :hover)
      :border "2px solid rgba(255,128,128,0.8)"

      (.debug-frame__header :display "inline-block"
                            :position "absolute"
                            :left "-2px"
                            :top "-32px"
                            :font-size "20px"
                            :line-height "20px"
                            :background-color "rgba(255,128,128,0.8)"
                            :padding "5px"
                            :margin "0px 0px -30px 0px"
                            :white-space "nowrap"
                            :border-top-left-radius "5px"
                            :border-top-right-radius "5px"))
     (.debug-frame
      :position "relative"
      :border "2px solid rgba(0,0,0,0)"
      :margin-top "-2px"
      :margin-left "-2px"
      :box-sizing "border-box")
     (.debug-frame__header :display "none"))))


#+sbcl
(defun get-widget-source (widget)
  (let ((source (sb-introspect:find-definition-source widget)))
    (when source
      (with-accessors ((pathname sb-introspect:definition-source-pathname)
                       (form-path sb-introspect:definition-source-form-path))
          source
        
        (with-open-file (stream pathname)
          (let ((form-number (car form-path))
                form)
            (dotimes (i form-number)
              (setf form
                    (read stream)))
            form))))))


(defvar *debug-widgets-structure* nil
  "Если true, то рисуем рамки вокруг виджетов.")


(defmethod render-widget :around ((widget widget) &rest args)
  (declare (ignorable args))
  (log:info "Rendering" widget)
  
  (when (equal (widget-name widget)
               "root")
    (with-html
      (:style (str (get-debug-style)))))
  
  (if *debug-widgets-structure*
      (with-html
        (:div :class "debug-frame"
              (:h1 :class "debug-frame__header" (esc (format nil "~A" widget)))
              (call-next-method)))
      (call-next-method)))


;; TODO: remove because new widget was created
;; (defwidget contacts-list ()
;;   ((current-contact :initform nil
;;                     :initarg :current-contact
;;                     :accessor current-contact)
;;    (details-widget :initform nil
;;                    :reader details-widget)))


;; (defmethod (setf current-contact) :after (new-current-contact (contacts-list contacts-list))
;;   (log:info "New contact was selected" new-current-contact)
;;   (setf (hacrm.widgets.contact-details:contact-details-contact
;;          (details-widget contacts-list))
;;         new-current-contact))


;; (defmethod initialize-instance :after ((contacts-list contacts-list) &rest args)
;;   "Создадим для списка контактов виджет, отображающий детали текущего контакта."
;;   (declare (ignorable args))
;;   (setf (slot-value contacts-list 'details-widget)
;;         (hacrm.widgets.contact-details:make-contact-details-widget
;;          contacts-list
;;          (current-contact contacts-list))))


;; (defun make-contacts-list ()
;;   "Создаёт виджет со списком контактов"
;;   (make-instance 'contacts-list))


;; (defun remove-contact (contact-list contact)
;;   (log:debug "Removing" contact)
  
;;   (weblocks-stores:delete-persistent-object
;;    *hacrm-store*
;;    contact)
  
;;   (mark-dirty contact-list))


;; (defun select-contact (contact-list contact)
;;   (log:debug "Selecting" contact)

;;   (setf (current-contact contact-list)
;;         contact)
;;   ;; (mark-dirty contact-list)
;;   )


;; (defun render-contacts (contact-list contacts)
;;   (with-html
;;     (:h1 "Контакты")
;;     (:table :class "table"
;;             (:tr (:th "Имя")
;;                  (:th :style "text-align: right" "Действие"))
;;             (loop for c in contacts
;;                   do (htm (:tr (:td (render-link (funcall (f_ (f_% (select-contact contact-list _)))
;;                                                           c)
;;                                                  (hacrm.models.contact:name c)))
;;                                (:td :style "text-align: right"
;;                                     (render-link (funcall (f_ (f_% (remove-contact contact-list _)))
;;                                                           c)
;;                                                  "Удалить"
;;                                                  :class "btn btn-danger btn-mini"))))))))

;; (defun render-contact-details (contact-list)
;;   "TODO: удалить и использовать вместо этого виджет."
;;   (let ((contact (current-contact contact-list)))
;;     (with-html
;;       (:h1 (esc (hacrm.models.contact:name contact)))
;;       (render-link (f_% (setf (current-contact contact-list)
;;                               nil)
;;                         (mark-dirty contact-list))
;;                    "Отмена"
;;                    :class "btn btn-default"))))


;; (defmethod render-widget-body ((contact-list contacts-list) &rest rest)
;;   "Returns HTML with contacts list."
  
;;   (declare (ignorable rest))
;;   (let ((directory (slot-value *hacrm-store* 'directory)))
;;     (log:info "Reading contacts from" directory))
  
;;   (let ((contacts (hacrm.models.contact:all-contacts)))
;;     (if contacts
;;         (with-html
;;           (:table :class "vbox"
;;                   :style "width: 100%"
;;                   (:tr :valign "top"
;;                    (:td :style "width: 30%; padding-right: 30px;"
;;                         (render-contacts contact-list contacts))
;;                    (:td
;;                         :style ""
;;                         (render-widget (details-widget contact-list))
;;                         ;; (hacrm.widgets.notes:render-contact-notes (current-contact contact-list))
;;                         ))))
;;         (with-html
;;           (:p "Пока нет ни одного контакта. Добавь хотя бы один.")))))


;; (defwidget custom-form ()
;;   ())


;; (defun make-custom-form (contacts-list)
;;   (make-instance 'custom-form
;;                  :propagate-dirty (list contacts-list)))


;; (defun add-new-contact (form &rest args)
;;   (let* ((name (getf args :name))
;;          (contact (hacrm.models.contact:make-contact name)))
    
;;     (log:debug "Adding new contact" args)

;;     (mark-dirty form)))


;; (defmethod render-widget-body ((form custom-form) &rest rest)
;;   (declare (ignorable rest))

;;   (with-html-form (:post (lambda (&rest args)
;;                            (apply #'add-new-contact
;;                                   form
;;                                   args)))
;;     (:h1 "Добавить новый контакт")
    
;;     (:p (:label :for "name"
;;                 "Имя")
;;         (:input :type "text"
;;                 :name "name"
;;                 :value ""))
;;     (:p (:input :type "submit"
;;                 :class "btn btn-primary"
;;                 :value "Добавить"))))


(defun init-user-session (root)
  (let ((main-window (hacrm.widgets.main:make-main-window)))
    
    (setf (widget-children root)
          (list main-window))))

