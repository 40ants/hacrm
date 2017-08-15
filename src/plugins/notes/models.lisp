(in-package hacrm.plugins.notes)


(def-feed-item note
    ((text :type string
           :initarg :text
           :accessor text)))


(defun make-note (text)
  "Returns a new note with given text."
  (make-instance 'note :text text))
