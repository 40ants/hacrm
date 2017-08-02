(in-package hacrm.plugins.notes)


(defmethod hacrm.widgets.feed:render-feed-item ((note note))
  (with-html
    (:div :class "note"
          (esc (text note)))))
