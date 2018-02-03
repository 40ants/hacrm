(defpackage #:hacrm.plugins.notes
  (:use #:cl
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
