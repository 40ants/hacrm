(defpackage #:hacrm-notes/widgets
  (:use #:cl)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:render
                #:defwidget)
  (:import-from #:hacrm/widgets/feed
                #:make-feed-item-widget)
  (:import-from #:local-time
                #:universal-to-timestamp
                #:format-rfc1123-timestring)
  (:import-from #:hacrm/utils
                #:render-markup)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/dependencies
                #:get-dependencies)
  (:import-from #:hacrm-notes/models
                #:note)
  (:import-from #:hacrm/models/feed
                #:created-at))
(in-package hacrm-notes/widgets)


(defwidget note-widget ()
  ((note :type note
         :initarg :note
         :reader note)))


(defmethod make-feed-item-widget ((note note)
                                  parent-widget)
  (declare (ignorable parent-widget))
  (make-instance 'note-widget :note note))


(defmethod render ((widget note-widget))
  (let* ((note (note widget))
         (created-at (created-at note))
         (formatted-date (format-rfc1123-timestring
                          nil
                          (universal-to-timestamp created-at)))
         (rendered-text (render-markup (hacrm-notes/models::text note))))
    (with-html
      (:p :class "note__metadata"
          (:span formatted-date))
      (:p :class "note__text"
          ;; Here we dont escape the text, because it already
          ;; processed by markup engine
          (:raw rendered-text)))))


(defmethod get-dependencies  ((widget note-widget))
  (list (weblocks-lass:make-dependency
         '(.note-widget
           :background-color "#F5F5F5"
           :padding "10px"
           :margin-bottom "20px"
           :border-radius "10px"))))
