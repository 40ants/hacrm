
;; пробуем евалить тесты

(defun catch-slime-notes (notes)
  "Display the log on failed compilations or if NOTES is non-nil."
  (message "Here are the notes: \"%s\"" notes))

(add-hook 'slime-compilation-finished-hook 'catch-slime-notes)
