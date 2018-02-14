(defpackage #:hacrm/widgets/help
  (:use #:cl)
  (:import-from #:hacrm/commands)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:hacrm/widgets/base
                #:base)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:export
   #:make-help-widget))
(in-package hacrm/widgets/help)


(defwidget help (base)
  ((commands :type list
             :initarg :commands
             :reader commands)))


(defmethod render ((widget help))
  (with-html
    (:h1 "Commands list")
    (:dl :class "help__commands"
         (loop for command in (commands widget)
               do (with-html
                    (:dt (first command))
                    (:dd (second command)))))))


(defun make-help-widget (previous-widget)
  "Creates a help widget to show commands list available for the given widget."
  (make-instance 'help
                 :commands (hacrm/commands:help previous-widget)))


(defmethod get-dependencies  ((widget help))
  (list (weblocks-lass:make-dependency
         '(.help__commands
           (dt :margin-bottom 10px)
           (dd :margin-bottom 20px)))))
