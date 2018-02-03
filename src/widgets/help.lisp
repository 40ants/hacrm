(defpackage #:hacrm.widgets.help
  (:use #:cl)
  (:export
   #:make-help-widget))
(in-package hacrm.widgets.help)


(weblocks/widget:defwidget help (hacrm.widgets.base:base)
  ((commands :type list
             :initarg :commands
             :reader commands)))


(defmethod weblocks/widget:render ((widget help))
  (weblocks/html:with-html
    (:h1 "Commands list")
    (:dl :class "help__commands"
         (loop for command in (commands widget)
               do (weblocks/html:with-html
                    (:dt (first command))
                    (:dd (second command)))))))


(defun make-help-widget (previous-widget)
  "Creates a help widget to show commands list available for the given widget."
  
  (make-instance 'help
                 :commands (hacrm.commands:help previous-widget)))


(defmethod weblocks/dependencies:get-dependencies  ((widget help))
  (list (weblocks-lass:make-dependency
         '(.help__commands
           (dt :margin-bottom 10px)
           (dd :margin-bottom 20px)))))
