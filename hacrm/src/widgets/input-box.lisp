(defpackage #:hacrm.widgets.input-box
  (:use #:cl
        #:weblocks)
  (:export
   #:make-input-box))
(in-package hacrm.widgets.input-box)


(defwidget input-box (hacrm.widgets.base:base)
  ())


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
              :value ""))))

