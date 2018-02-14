(defpackage #:hacrm/widgets/input-box
  (:use #:cl)
  (:import-from #:hacrm/widgets/base)
  (:import-from #:weblocks-ui/form
                #:render-link
                #:with-html-form)
  (:import-from #:hacrm/commands
                #:process-query)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:export
   #:make-input-box))
(in-package hacrm/widgets/input-box)


(defwidget counter-box ()
  ((counter :initform nil
            :accessor counter
            :affects-dirty-status-p t)))


(defwidget input-box (hacrm/widgets/base:base)
  ((counter :initform (make-instance 'counter-box)
            :reader counter)))



(defmethod initialize-instance ((instance counter-box) &rest restargs)
  (declare (ignorable restargs))
  (call-next-method))


(defun make-input-box (window)
  (log:warn "Creating input-box")
  (make-instance 'input-box
                 :window window))



(defmethod weblocks/widget:render ((widget input-box))
  (flet ((process-user-input (&key query &allow-other-keys)
           (let* ((app-window (hacrm/widgets/base:window widget)))
             (process-query app-window query))))
    (with-html-form (:post #'process-user-input)
      (:input :type "text"
              :name "query"
              :autofocus t
              :placeholder "Enter a command or query here."
              :value "")
      (:div :style "width:40; height:40"
            (weblocks/widget:render (counter widget))))))


(defmethod weblocks/widget:render ((widget counter-box))
  (weblocks/html:with-html
    (:div (if (counter widget)
              (weblocks/html:with-html (:span (format nil "~a" (counter widget))))
              (render-link
               (lambda (&rest args)
                 (declare (ignorable args))
                 (setf (counter widget)
                       0)

                 ;; (weblocks.websocket:in-thread ("Update counter")
                 ;;   (sleep 3)
                 ;;   (log:info "Updating counter")
                 ;;   (incf (counter widget)))
                 )
               "Start counter")))))


(defmethod weblocks/dependencies:get-dependencies ((widget input-box))
  (append (list (weblocks-lass:make-dependency
                 '(.input-box
                   ((:or form input)
                    ;; bootstrap.css changes margin for forms and inputs
                    ;; and I need to change it back
                    :margin 0)
                   (input
                    :width 100%
                    :font-size 40px
                    :line-height 60px
                    :height 60px
                    :border-top 1px solid "#E0E0E0"
                    :border-radius 0
                    :background "#F5F5F5"
                    ;; remove blue glow around active input box
                    :outline none)
                   ((:and input :focus)
                    :background black
                    :color white))))
          (call-next-method)))


(defmethod weblocks/dependencies:get-dependencies ((widget counter-box))
  (append (list (weblocks-lass:make-dependency
                  '(.counter-box
                    :position fixed
                    :bottom 0
                    :right 0)))
          (call-next-method)))
