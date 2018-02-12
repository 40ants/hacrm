(defpackage #:hacrm/widgets/base
  (:use #:cl)
  (:export
   #:window
   #:base))
(in-package hacrm/widgets/base)


(weblocks/widget:defwidget base ()
  ((window :initarg :window
           :accessor window
           :documentation "Stores a pointer to a widget representing main window.")))
