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
  (:import-from #:weblocks/hooks
                #:on-application-hook-action
                #:call-next-hook)
  (:import-from #:weblocks/response
                #:send-script)
  (:export
   #:make-input-box))
(in-package hacrm/widgets/input-box)


(defwidget input-box (hacrm/widgets/base:base)
  ())


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
              :onblur "this.focus()"
              :placeholder "Enter a command or a search query here."
              :value ""))))


(on-application-hook-action
  return-focus-to-input ()
  
  (call-next-hook)
  
  (send-script
   "jQuery(\".input-box input[name='query']\").focus()"))


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
