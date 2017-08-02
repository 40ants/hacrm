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
   #:find-object))
(in-package #:hacrm.utils)


(defun store-object (object)
  (weblocks-stores:persist-object hacrm::*hacrm-store*
                                  object))

(defun remove-object (object)
  (weblocks-stores:delete-persistent-object hacrm::*hacrm-store*
                                            object))

(defun find-object (class-name)
  (weblocks-stores:find-persistent-objects hacrm::*hacrm-store* 
                                           class-name))

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
