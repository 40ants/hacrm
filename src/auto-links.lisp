(defpackage #:3bmd-auto-links
  (:nicknames #:hacrm/auto-links)
  (:use :cl :esrap :3bmd-ext)
  (:export #:*auto-links*))

(in-package #:3bmd-auto-links)


(defstruct auto-link
  url
  prefix
  suffix)


(define-extension-inline *auto-links* auto-link
    (and
     (? "(")
     (or "https" "http")
     "://"
     (+ (not (or 3bmd-grammar::space
                 3bmd-grammar::newline
                 #\))))
     (? ")"))

  (:destructure (prefix schema delimiter href suffix)
                (declare (ignorable delimiter))

                (list :auto-link (make-auto-link
                                  :url (text (format nil "~a://~a"
                                                     schema
                                                     (coerce href
                                                             'string)))
                                  :prefix (text prefix)
                                  :suffix (text suffix)))))



(defmethod print-tagged-element ((tag (eql :auto-link)) stream rest)
  (let ((link (first rest)))
    (format stream "~A<a href=\"~A\">~A</a>~A"
            (auto-link-prefix link)
            (auto-link-url link)
            (auto-link-url link)
            (auto-link-suffix link))))
