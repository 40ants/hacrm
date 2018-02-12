(defpackage #:hacrm/hooks
  (:use #:cl)
  (:import-from #:weblocks/hooks
                #:defhook))
(in-package hacrm/hooks)

(defhook feed-item-created)
(defhook fact-created)
(defhook fact-updated)
(defhook fact-removed)
