(defpackage #:hacrm.widgets.input-box
  (:use #:cl
        #:weblocks)
  (:import-from #:cl-who
                #:esc
                #:htm)
  (:export
   #:make-input-box))
(in-package hacrm.widgets.input-box)


(defwidget counter-box (weblocks.websocket:websocket-widget)
  ((counter :initform nil
            :accessor counter
            :affects-dirty-status-p t)))


(defwidget input-box (weblocks.websocket:websocket-widget hacrm.widgets.base:base)
  ((counter :initform (make-instance 'counter-box)
            :reader counter)))



(defmethod initialize-instance ((instance counter-box) &rest restargs)
  (declare (ignorable restargs))
  (call-next-method))


(defun make-input-box (window)
  (log:warn "Creating input-box")
  (make-instance 'input-box
                 :window window))



(defmethod render-widget-body ((widget input-box)
                               &rest rest)
  (declare (ignorable rest))
  
  (flet ((process-user-input (&key query &allow-other-keys)
           (let* ((app-window (hacrm.widgets.base:window widget)))
             (hacrm.commands:process-query app-window query))))
    (weblocks.ui.form:with-html-form (:post #'process-user-input)
      (:input :type "text"
              :name "query"
              :autofocus t
              :placeholder "Enter a command or query here."
              :value "")
      (:div :width 40 :height 40
            (render-widget (counter widget))))))


(defmethod render-widget-body ((widget counter-box)
                               &rest rest)
  (declare (ignorable rest))

  (with-html
    (:div (if (counter widget)
              (with-html (:span (esc (format nil "~a" (counter widget)))))
              (weblocks.ui.form:render-link
               (lambda (&rest args)
                 (declare (ignorable args))
                 (setf (counter widget)
                       0)

                 (weblocks.websocket:in-thread ("Update counter")
                   (sleep 3)
                   (log:info "Updating counter")
                   (incf (counter widget))))
               "Start counter")))))


(defmethod weblocks.dependencies:get-dependencies ((widget input-box))
  (append (list (weblocks.lass:make-dependency
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


(defmethod weblocks.dependencies:get-dependencies ((widget counter-box))
  (append (list (weblocks.lass:make-dependency
                  '(.counter-box
                    :position fixed
                    :bottom 0
                    :right 0)))
          (call-next-method)))
