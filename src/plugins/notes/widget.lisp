(in-package hacrm.plugins.notes)


(weblocks/widget:defwidget note-widget ()
  ((note :type note
         :initarg :note
         :reader note)))


(defmethod hacrm.widgets.feed:make-feed-item-widget ((note note)
                                                     parent-widget)
  (declare (ignorable parent-widget))
  (make-instance 'note-widget :note note))


(defmethod weblocks/widget:render ((widget note-widget))
  (let* ((note (note widget))
         (created-at (created-at note))
         (formatted-date (local-time:format-rfc1123-timestring
                          nil
                          (local-time:universal-to-timestamp created-at)))
         (rendered-text (hacrm.utils:render-markup (text note))))
    (weblocks/html:with-html
      (:p :class "note__metadata"
          (:span formatted-date))
      (:p :class "note__text"
          ;; Here we dont escape the text, because it already
          ;; processed by markup engine
          (:raw rendered-text)))))


(defmethod weblocks/dependencies:get-dependencies  ((widget note-widget))
  (list (weblocks-lass:make-dependency
         '(.note-widget
           :background-color "#F5F5F5"
           :padding "10px"
           :margin-bottom "20px"
           :border-radius "10px"))))
