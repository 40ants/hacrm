(defpackage #:hacrm.widgets.input-box
  (:use #:cl
        #:weblocks)
  (:export
   #:make-input-box))
(in-package hacrm.widgets.input-box)


(defwidget input-box (hacrm.widgets.base:base)
  ())


(defun make-input-box (window)
  (make-instance 'input-box
                 :window window))


(defmethod render-widget-body ((widget input-box)
                               &rest rest)
  (declare (ignorable rest))
  
  (flet ((process-user-input (&key query &allow-other-keys)
           (let* ((app-window (hacrm.widgets.base:window widget)))
             (hacrm.commands:process-query app-window query))))
    
    (with-html-form (:post #'process-user-input)
      (:input :type "text"
              :name "query"
              :autofocus t
              :placeholder "Enter a command or query here."
              :value ""))))


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


