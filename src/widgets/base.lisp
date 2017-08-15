(defpackage #:hacrm.widgets.base
  (:use #:cl
        #:weblocks)
  (:export
   #:window
   #:base))
(in-package hacrm.widgets.base)


(defwidget base ()
  ((window :initarg :window
           :accessor window
           :documentation "Stores a pointer to a widget representing main window.")))
