(defpackage #:hacrm.widgets.help
  (:use #:cl
        #:weblocks
        #:cl-who)
  (:export
   #:make-help-widget))
(in-package hacrm.widgets.help)


(defwidget help (hacrm.widgets.base:base)
  ((commands :type list
             :initarg :commands
             :reader commands)))


(defmethod render-widget-body ((widget help) &rest rest)
  (declare (ignore rest))

  (with-html
    (:h1 "Commands list")
    (:dl :class "help__commands"
     (loop for command in (commands widget)
           do (with-html
                (:dt (esc (first command)))
                (:dd (esc (second command))))))))


(defun make-help-widget (previous-widget)
  "Creates a help widget to show commands list available for the given widget."
  
  (make-instance 'help
                 :commands (hacrm.commands:help previous-widget)))


(defmethod weblocks.dependencies:get-dependencies  ((widget help))
  (list (weblocks.lass:make-dependency
         '(.help__commands
           (dt :margin-bottom 10px)
           (dd :margin-bottom 20px)))))
