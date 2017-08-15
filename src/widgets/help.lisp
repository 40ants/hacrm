(defpackage #:hacrm.widgets.help
  (:use #:cl #:weblocks)
  (:export
   #:make-help-widget))
(in-package hacrm.widgets.help)


(defwidget help (hacrm.widgets.base:base)
  ())


(defmethod render-widget-body ((widget help) &rest rest)
  (declare (ignore rest))

  (with-html
    (:p "Nothing here")))


(defun make-help-widget ()
  (make-instance 'help))
