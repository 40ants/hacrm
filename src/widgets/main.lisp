(defpackage #:hacrm/widgets/main
  (:use #:cl)
  (:export
   #:make-main-window
   #:change-widget
   #:main-widget
   #:input-box
   #:reset-user-input))
(in-package hacrm/widgets/main)


(weblocks/widget:defwidget main-window ()
  ((input-box :type (or input-box
                        null)
              :initarg :input-box
              :reader input-box)
   (main-widget :type (or hacrm/widgets/base:base
                          null)
                :initarg :main-widget
                :accessor main-widget)))


(defmethod weblocks/dependencies:get-dependencies ((widget main-window))
  ;; http://css-live.ru/articles/vizualnoe-rukovodstvo-po-svojstvam-flexbox-iz-css3.html
  (append (list (weblocks-lass:make-dependency
                 '(.main-window
                   :position absolute
                   :width 100%
                   :height 100%
                   :display flex
                   :flex-direction column
                   
                   (.main-window__working-area
                    :flex 1 1 100%
                    :overflow auto
                    :padding 10px)
                   
                   (.main-window__input
                    :flex-shrink 0)
                   )))
          (call-next-method)))


(defun make-main-window ()
  (let* ((help-widget (hacrm/widgets/help:make-help-widget nil))
         (app-window (make-instance 'main-window
                                :main-widget help-widget))
         (input-box (hacrm/widgets/input-box:make-input-box app-window))
         ;; (help-widget (hacrm/widgets/help:make-help-widget window))
         )
    (setf (slot-value app-window 'input-box)
          input-box)

    ;; Now we need to link back from the current widget to the app window
    (setf (hacrm/widgets/base:window help-widget)
          app-window)

    app-window))


(defmethod weblocks/widget:render ((window main-window))
  (log:info "Rendering main window")
  
  (weblocks/html:with-html
    (:div :class "main-window__working-area"
          (weblocks/widget:render (main-widget window)))
    (:div :class "main-window__input"
          (weblocks/widget:render (input-box window)))))


(defun change-widget (current-widget new-widget)
  "Changes main widget of the application."
  (let ((app-window (hacrm/widgets/base:window current-widget)))
    (setf (main-widget app-window)
          new-widget)

    ;; We need to link back from new widget to the app window
    (setf (hacrm/widgets/base:window new-widget)
          app-window)

    (weblocks/widget:update app-window)))


(defun reset-user-input (current-widget)
  (let* ((app-window (hacrm/widgets/base:window current-widget))
         (input-box (input-box app-window)))
    (weblocks/widget:update input-box)))


(defmethod hacrm/commands:command ((window main-window) command query)
  "This method is a proxy, used to pass call to currently active main widget."

  (hacrm/commands:command (main-widget window) command query))
