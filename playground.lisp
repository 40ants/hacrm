(defpackage #:playground
  (:use #:cl))
(in-package #:playground)


;; Проверим куда пропали теги
(defvar all-users (hacrm.models.contact:all-contacts))
(defvar all-tags (hacrm.plugins.tags:get-all-tags))

(defvar bound-tags (loop for contact in all-users
               for contact-tags = (hacrm.plugins.tags:get-contact-tags contact)
               appending contact-tags))
(defvar missing-tags (remove-if (lambda (tag)
                                  (member tag bound-tags))
                                all-tags))

(defvar tags-missing-because-nil-contact
  (remove-if-not (lambda (tag)
               (null (hacrm.plugins.tags:contact tag)))
             missing-tags))


;; Проверим куда пропали заметки
(defvar all-notes (hacrm.plugins.notes:get-all-notes))

(defvar bound-notes
  (loop for contact in all-users
        for contact-notes = (hacrm.plugins.notes:get-contact-notes contact)
        appending contact-notes))

(defvar missing-notes (remove-if (lambda (note)
                                  (member note bound-notes))
                                all-notes))

(defvar notes-missing-because-nil-contact
  (remove-if-not
   (lambda (note)
     (null (hacrm.plugins.notes:get-object note)))
   missing-notes))
