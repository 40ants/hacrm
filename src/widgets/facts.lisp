(defpackage #:hacrm/widgets/facts
  (:use #:cl)
  (:export
   #:make-facts-group-widget
   #:fact-group-weight))
(in-package hacrm/widgets/facts)


(defgeneric make-facts-group-widget (fact-group object)
  (:documentation "Returns a widget to render grouped together
facts about given object.

`Fact-group' is a keyword identifying the group. It should be one
of keywords, returned by `hacrm/models/facts/core:fact-groups' function."))


(defgeneric fact-group-weight (fact-group-widget)
  (:documentation "Accepts a widget for representing a group of facts and returns a number.

This number will be used to sort widgets in contact details widget.
Lesser number – higher this widget will be shown.

By default, all widget groups all have same weight of 10.")
  (:method (fact-group-widget)
    (declare (ignorable fact-group-widget))
    10))


