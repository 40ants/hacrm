(defpackage #:hacrm.plugins.notes
  (:use #:cl
        #:weblocks
        #:cl-who
        #:hacrm.models.feed
        #:hacrm.models.core
        #:f-underscore)
  (:export
   #:note
   #:make-note
   #:add-note
   #:remove-note
   #:get-notes
   #:get-object
   #:text))
