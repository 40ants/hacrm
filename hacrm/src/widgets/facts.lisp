(defpackage #:hacrm.widgets.facts
  (:use #:cl)
  (:export
   #:make-facts-group-widget))
(in-package hacrm.widgets.facts)


(defgeneric make-facts-group-widget (fact-group object)
  (:documentation "Returns a widget to render grouped together
facts about given object.

`Fact-group' is a keyword identifying the group. It should be one
of keywords, returned by `hacrm.models.facts.core:fact-groups' function."))
