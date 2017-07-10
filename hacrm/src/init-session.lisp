(in-package hacrm)


(defwidget list-dependencies ()
  ())


(defmethod render-widget-body ((widget list-dependencies) &rest args)
  (declare (ignorable args))
  (with-html
    (:ul :class "page-dependencies"
         (loop :for item :in (append (webapp-application-dependencies)
                                     weblocks::*page-dependencies*)
               :do (htm
                    (:li (esc (format nil "dep: ~s" item))))
               )
         )))



;; (defmethod render-widget-body ((widget list-dependencies) &rest args)
;;   (declare (ignorable args))
;;   (with-html
;;     (:ul :class "page-dependencies"
;;          (loop :for item :in (append (webapp-application-dependencies)
;;                                      weblocks::*page-dependencies*)
;;                :do (htm
;;                     (:li (esc (format nil "dep: ~s" item))))
;;                ))))


(defwidget github-projects ()
  ((username :type string)))


(defmethod render-widget-body ((widget github-projects) &rest args)
  (declare (ignorable args))
  
  (with-html
    (:table :class "table"
     (:tr
      (:td "Hello")
      (:td "World2!")))))


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


(defwidget contacts-list ()
  ((current-contact :initform nil
                    :initarg :current-contact
                    :accessor current-contact)
   (details-widget :initform nil
                   :reader details-widget)))


(defmethod (setf current-contact) :after (new-current-contact (contacts-list contacts-list))
  (log:info "New contact was selected" new-current-contact)
  (setf (hacrm.widgets.contact-details:contact-details-contact
         (details-widget contacts-list))
        new-current-contact))


(defmethod initialize-instance :after ((contacts-list contacts-list) &rest args)
  "Создадим для списка контактов виджет, отображающий детали текущего контакта."
  (declare (ignorable args))
  (setf (slot-value contacts-list 'details-widget)
        (hacrm.widgets.contact-details:make-contact-details-widget
         contacts-list
         (current-contact contacts-list))))


(defun make-contacts-list ()
  "Создаёт виджет со списком контактов"
  (make-instance 'contacts-list))


(defun remove-contact (contact-list contact)
  (log:debug "Removing" contact)
  
  (weblocks-stores:delete-persistent-object
   *hacrm-store*
   contact)
  
  (mark-dirty contact-list))


(defun select-contact (contact-list contact)
  (log:debug "Selecting" contact)

  (setf (current-contact contact-list)
        contact)
  ;; (mark-dirty contact-list)
  )


(defun render-contacts (contact-list contacts)
  (with-html
    (:h1 "Контакты")
    (:table :class "table"
            (:tr (:th "Имя")
                 (:th :style "text-align: right" "Действие"))
            (loop for c in contacts
                  do (htm (:tr (:td (render-link (funcall (f_ (f_% (select-contact contact-list _)))
                                                          c)
                                                 (hacrm.models.contact:name c)))
                               (:td :style "text-align: right"
                                    (render-link (funcall (f_ (f_% (remove-contact contact-list _)))
                                                          c)
                                                 "Удалить"
                                                 :class "btn btn-danger btn-mini"))))))))

(defun render-contact-details (contact-list)
  "TODO: удалить и использовать вместо этого виджет."
  (let ((contact (current-contact contact-list)))
    (with-html
      (:h1 (esc (hacrm.models.contact:name contact)))
      (render-link (f_% (setf (current-contact contact-list)
                              nil)
                        (mark-dirty contact-list))
                   "Отмена"
                   :class "btn btn-default"))))


(defmethod render-widget-body ((contact-list contacts-list) &rest rest)
  "Returns HTML with contacts list."
  
  (declare (ignorable rest))
  (let ((directory (slot-value *hacrm-store* 'directory)))
    (log:info "Reading contacts from" directory))
  
  (let ((contacts (hacrm.models.contact:find-contacts)))
    (if contacts
        (with-html
          (:table :class "vbox"
                  :style "width: 100%"
                  (:tr :valign "top"
                   (:td :style "width: 30%; padding-right: 30px;"
                        (render-contacts contact-list contacts))
                   (:td
                        :style ""
                        (render-widget (details-widget contact-list))
                        ;; (hacrm.widgets.notes:render-contact-notes (current-contact contact-list))
                        ))))
        (with-html
          (:p "Пока нет ни одного контакта. Добавь хотя бы один.")))))


(defwidget custom-form ()
  ())


(defun make-custom-form (contacts-list)
  (make-instance 'custom-form
                 :propagate-dirty (list contacts-list)))


(defun add-new-contact (form &rest args)
  (let* ((name (getf args :name))
         (contact (hacrm.models.contact:make-contact name)))
    
    (log:debug "Adding new contact" args)

    (weblocks-stores:persist-object *hacrm-store*
                                    contact)
    (mark-dirty form)))


(defmethod render-widget-body ((form custom-form) &rest rest)
  (declare (ignorable rest))

  (with-html-form (:post (lambda (&rest args)
                           (apply #'add-new-contact
                                  form
                                  args)))
    (:h1 "Добавить новый контакт")
    
    (:p (:label :for "name"
                "Имя")
        (:input :type "text"
                :name "name"
                :value ""))
    (:p (:input :type "submit"
                :class "btn btn-primary"
                :value "Добавить"))))


(defwidget input-box ()
  ((callback :type function
             :initarg :callback
             :reader callback)))


(defmethod weblocks.dependencies:get-dependencies ((widget input-box))
  (append (list (weblocks.lass:make-dependency
                  '(.input-box
                    (form
                     :margin 0)
                    (input
                     :margin 0
                     :width "100%"
                     :font-size "30px"
                     :height "30px"
                     :border 0
                     :border-top "1px solid gray"
                     :border-radius 0
                     ))))
          (call-next-method)))


(defun make-input-box (callback)
  (make-instance 'input-box
                 :callback callback))


(defmethod render-widget-body ((widget input-box)
                               &rest rest)
  (declare (ignorable rest))
  
  (with-html-form (:post (lambda (&rest args)
                           (apply (callback widget)
                                  args)))
    (:input :type "text"
            :name "query"
            :autofocus t
            :value "")))


(defwidget main-window ()
  ((input-box :type input-box
              :initarg :input-box
              :reader get-input-box)
   (main-widget :type widget
                :initarg :main-widget
                :accessor main-widget)))


(defmethod weblocks.dependencies:get-dependencies ((widget main-window))
  (append (list (weblocks.lass:make-dependency
                  '(.main-window
                    :position absolute
                    :left 0
                    :right 0
                    :top 0
                    :bottom 0
                    :display flex
                    :flex-direction column
                    (.main-window__working-area
                     :flex 1 1 100%
                     :padding 10px)
                    (.main-window__input
                     :flex-shrink 0)
                    )))
          (call-next-method)))


(defun rewrite-query (query)
  (if (find #\: query)
      query
      (format nil "name:~a or tag:~a" query query)))


(defmethod hacrm.query:process-query ((widget widget)
                                      token
                                      query)
  "If no handler processed the query, then we'll try to search a contact."
  (declare (ignorable token))

  (log:debug "Trying to search contact" query)
  
  (let* ((search-query (rewrite-query query))
         (contacts (hacrm.search:search-contact search-query))
         (contacts-count (length contacts)))
    (log:debug "Search completed" contacts-count)
    
    (cond
      ((eql contacts-count 0)
       (make-widget "No contacts were found"))
      ((eql contacts-count 1)
       (hacrm.widgets.contact-details:make-contact-details2-widget
        (car contacts)))
      (t
       (hacrm.widgets.contacts-list:make-contacts-list
        contacts)))))


(defmethod hacrm.query:process-query ((widget widget)
                                      (token (eql :contacts))
                                      query)
  "Shows full contact list."
  (declare (ignorable query))
  (hacrm.widgets.contacts-list:make-contacts-list
   (hacrm.models.contact:find-contacts)))


(defun make-main-window ()
  (let (window)
    (flet ((callback (&key query &allow-other-keys)
             (log:debug "Callback called" query)
             (let* ((current-widget (main-widget window))
                    (widget (hacrm.query:process-string-query current-widget query)))
               (when (and widget
                          (not (eql current-widget
                                    widget)))
                 (setf (main-widget window)
                       widget)
                 (mark-dirty window)))))
    
      (setf window
            (make-instance 'main-window
                           :input-box (make-input-box #'callback)
                           :main-widget (make-widget "Enter the query"))))))


(defmethod render-widget-body ((window main-window)
                               &rest rest)
  (declare (ignorable rest))
  
  (with-html
    (:div :class "main-window__working-area"
          (render-widget (main-widget window)))
    (:div :class "main-window__input"
          (render-widget (get-input-box window)))))


(defun init-user-session (root)
  (let ((contacts-list (make-contacts-list)))
    (setf (widget-children root)
          (list (make-main-window)
                ;; contacts-list
                ;; (make-custom-form contacts-list)
                ))))

