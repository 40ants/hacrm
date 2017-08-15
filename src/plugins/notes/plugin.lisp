(defpackage #:hacrm.plugins.notes
  (:use #:cl
        #:weblocks
        #:cl-who
        #:hacrm.models.feed
        #:f-underscore)
  (:export
   #:note
   #:make-note))
