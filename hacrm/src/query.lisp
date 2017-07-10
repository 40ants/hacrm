(defpackage #:hacrm.query
  (:use #:cl)
  (:export
   #:process-query
   #:process-string-query))
(in-package hacrm.query)


(defgeneric process-query (widget first-token query)
  (:documentation "Processes given query and returns a given widget or a new one.")
  (:method (widget
            first-token
            query)
    (declare (ignorable widget first-token query))
    (log:debug "Query was ignored" query)
    widget))


(defun process-string-query (widget query)
  "Processes user's input depending on current widget.

To hook into this system, define a process-query method
specified on widget and symbolic name of the first token
from the query"

  (log:debug "Processing" query)
  
  (let* ((tokens (cl-strings:split query #\Space))
         (first-token-str (car tokens))
         (first-token (intern (string-upcase first-token-str)
                              :keyword)))
    (process-query widget first-token query)))

