(defpackage  #:hacrm.utils
  (:use #:cl)
  ;; (:import-from #:weblocks-utils
  ;;               #:all-of)
  (:export #:store-object
;;b           #:all-of
           #:format-time
           #:first-line
           #:text->html)
  (:export
   #:remove-object
   #:find-object
   #:render-markup
   #:parse-time))
(in-package #:hacrm.utils)


(defun store-object (object)
  (error "Need to reimplement")
  (weblocks-stores:persist-object hacrm::*store*
                                  object))

(defun remove-object (object)
  (error "Need to-reimplement using transactions")
  (weblocks-stores:delete-persistent-object hacrm::*store*
                                            object))

(defun find-object (root-object-name &key filter)
  (let ((objects (cl-prevalence:get-root-object hacrm::*store*
                                                root-object-name)))
    (if filter
        (remove-if-not filter objects)
        objects)))


(defun parse-time (string)
  (let ((universal (cl-date-time-parser:parse-date-time string)))
    (local-time:universal-to-timestamp universal)))


(defun format-time (universal-time)
  "Возвращает строку в формате YYYY-MM-DD hh:mm"
  
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp universal-time)
   :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2))))


(defun first-line (text)
  (first (split-sequence:split-sequence #\Newline text)))


(defun text->html (text)
  "Renders text in Markdown markup into HTML using all necessary extensions."
  
  (multiple-value-bind (document html)
      (cl-markdown:markdown text
                            :stream nil
                            :additional-extensions
                            '(cl-markdown:anchor)
                            )
    (declare (ignorable document))
    html))


(defun render-markup (text)
  "Transforms a text in cl-markup into a HTML and returns a new string."
  (let ((3bmd-auto-links:*auto-links* t))
    (with-output-to-string (s)
      (3bmd:parse-string-and-print-to-stream text s))))
