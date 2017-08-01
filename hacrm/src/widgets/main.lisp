(defpackage #:hacrm.widgets.main
  (:use #:cl #:weblocks)
  (:export
   #:make-main-window
   #:change-widget
   #:main-widget
   #:input-box
   #:reset-user-input))
(in-package hacrm.widgets.main)


(defwidget main-window ()
  ((input-box :type (or input-box
                        null)
              :initarg :input-box
              :reader input-box)
   (main-widget :type (or hacrm.widgets.base:base
                          null)
                :initarg :main-widget
                :accessor main-widget)))


(defmethod weblocks.dependencies:get-dependencies ((widget main-window))
  ;; http://css-live.ru/articles/vizualnoe-rukovodstvo-po-svojstvam-flexbox-iz-css3.html
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


(defun make-main-window ()
  (let* ((help-widget (hacrm.widgets.help:make-help-widget))
         (app-window (make-instance 'main-window
                                :main-widget help-widget))
         (input-box (hacrm.widgets.input-box:make-input-box app-window))
         ;; (help-widget (hacrm.widgets.help:make-help-widget window))
         )
    (setf (slot-value app-window 'input-box)
          input-box)

    ;; Now we need to link back from the current widget to the app window
    (setf (hacrm.widgets.base:window help-widget)
          app-window)

    app-window))


(defmethod render-widget-body ((window main-window)
                               &rest rest)
  (declare (ignorable rest))
  (log:error "Rendering main window")
  
  (with-html
    (:div :class "main-window__working-area"
          (render-widget (main-widget window)))
    (:div :class "main-window__input"
          (render-widget (input-box window)))))


(defun change-widget (current-widget new-widget)
  "Changes main widget of the application."
  (let ((app-window (hacrm.widgets.base:window current-widget)))
    (setf (main-widget app-window)
          new-widget)

    ;; We need to link back from new widget to the app window
    (setf (hacrm.widgets.base:window new-widget)
          app-window)

    (mark-dirty app-window)))


(defun reset-user-input (current-widget)
  (let* ((app-window (hacrm.widgets.base:window current-widget))
         (input-box (input-box app-window)))
    (weblocks:mark-dirty input-box)))


(defmethod hacrm.commands:command ((window main-window) command query)
  "This method is a proxy, used to pass call to currently active main widget."

  (hacrm.commands:command (main-widget window) command query))
