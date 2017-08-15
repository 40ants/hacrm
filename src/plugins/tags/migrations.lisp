(defpackage #:hacrm.plugins.tags.migrations
  (:use #:cl)
  (:export
   #:move-tags-to-plugin-package))
(in-package hacrm.plugins.tags.migrations)


;; (defun remove-old-tags ()
;;   (let ((old-tags (weblocks-stores:find-persistent-objects
;;                    hacrm::*hacrm-store*
;;                    'hacrm.models.facts.tag:tag)))
;;     (dolist (tag old-tags)
;;       (weblocks-stores:delete-persistent-object hacrm::*hacrm-store*
;;                                                 tag))))


;; (defun rename-object-class ()
;;   (let ((old-tags (weblocks-stores:find-persistent-objects
;;                    hacrm::*hacrm-store*
;;                    'hacrm.models.facts.tag:tag)))
;;     (dolist (old-tag old-tags)
;;       (hacrm.utils:store-object
;;        (hacrm.plugins.tags:make-tag-fact
;;         (hacrm.models.facts.core:contact old-tag)
;;         (hacrm.models.facts.tag:name old-tag))))))


(defun remove-old-tags ()
  (let ((old-tags (weblocks-stores:find-persistent-objects
                   hacrm::*hacrm-store*
                   'hacrm.plugins.tags::new-tag)))
    (dolist (tag old-tags)
      (weblocks-stores:delete-persistent-object hacrm::*hacrm-store*
                                                tag))))


(defun rename-object-class ()
  (let ((old-tags (weblocks-stores:find-persistent-objects
                   hacrm::*hacrm-store*
                   'hacrm.plugins.tags::new-tag)))
    (dolist (old-tag old-tags)
      (hacrm.utils:store-object
       (hacrm.plugins.tags:make-tag-fact
        (hacrm.models.facts.core:contact old-tag)
        (hacrm.plugins.tags:name old-tag))))))


(defun move-tags-to-plugin-package ()
  (rename-object-class)
  (remove-old-tags)
  (cl-prevalence:snapshot hacrm::*hacrm-store*))
