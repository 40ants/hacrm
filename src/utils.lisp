(defpackage  #:hacrm/utils
  (:use #:cl)
  (:import-from #:cl-date-time-parser
                #:parse-date-time)
  (:import-from #:local-time
                #:format-timestring
                #:universal-to-timestamp)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cl-markdown
                #:markdown)
  (:import-from #:3bmd)
  (:import-from #:hacrm/auto-links
                #:*auto-links*)
  (:export #:format-time
           #:first-line
           #:text->html)
  (:export
   #:remove-object
   #:find-object
   #:render-markup
   #:parse-time))
(in-package hacrm/utils)


(defun parse-time (string)
  (let ((universal (parse-date-time string)))
    (universal-to-timestamp universal)))


(defun format-time (universal-time)
  "Возвращает строку в формате YYYY-MM-DD hh:mm"
  
  (format-timestring
   nil
   (universal-to-timestamp universal-time)
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2))))


(defun first-line (text)
  (first (split-sequence #\Newline text)))


(defun text->html (text)
  "Renders text in Markdown markup into HTML using all necessary extensions."
  
  (multiple-value-bind (document html)
      (markdown text
                :stream nil
                :additional-extensions
                '(cl-markdown:anchor)
                )
    (declare (ignorable document))
    html))


(defun render-markup (text)
  "Transforms a text in cl-markup into a HTML and returns a new string."
  (let ((*auto-links* t))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream text s))))
