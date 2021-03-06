(defpackage #:hacrm/plugins/email/imap
  (:use #:cl)
  (:import-from #:ubiquitous)
  (:import-from #:mel-base)
  (:import-from #:cl-rfc2047)
  (:import-from #:sanitize)
  ;; This module brings :sf.mel package
  (:import-from #:hacrm/plugins/email/multiparts))
(in-package hacrm/plugins/email/imap)


;; Пока устанавливаем параметры таким образом:

(defun set-password (password)
  (setf (ubiquitous:value :hacrm/plugins/email :accounts)
        `((:host "imap.yandex.ru" :username "art@allmychanges.com" :password ,password))))


(defun read-headers (message)
  (let ((stream (mel.public:message-header-stream message)))
    (unwind-protect
         (mel.mime:read-rfc2822-header stream)
      (close stream))))


(defun parse-from-address (text)
  (let* ((decoded-text (cl-rfc2047:decode* text))
         (address (mel.mime:parse-rfc2822-address decoded-text))
         (name (sf.mel:eml-address->name address))
         (email (sf.mel:eml-address->email address)))
    (list :name name :email email)))


(defun trim-newlines-and-spaces (text)
  (string-trim '(#\Newline #\Return #\Space) text))


(defun normalize-newlines (text)
  (cl-strings:replace-all text
                          (coerce '(#\Return #\Newline) 'string)
                          (string #\Newline)))


(defun get-usable-text (message)
  (let* ((parts (mel.mime:parts message))
         (html-parts (remove-if-not #'sf.mel:part-body-html? parts))
         (text-parts (remove-if-not #'sf.mel:part-body-text? parts))
         (text (cond
                 (html-parts (sf.mel:part-body-html (first html-parts)))
                 (text-parts (sf.mel:part-body-text (first text-parts)))
                 (t nil))))
    (when text
      (normalize-newlines
       (trim-newlines-and-spaces
        (sanitize:clean text sanitize:+basic+))))))


(defun fetch-message (message)
  "Returns a list with two items:

1) Email author's name and email like (:name \"Some Author\" :email \"author@example.com\")
2) Sanitized message body."
  
  (let* ((text (get-usable-text message))
         (headers (read-headers message))
         (from (alexandria:assoc-value headers :from))
         (message-id (alexandria:assoc-value headers :message-id))
         (subject (alexandria:assoc-value headers :subject))
         (time (alexandria:assoc-value headers :date)))

    (list :from (parse-from-address from)
          :subject (cl-rfc2047:decode* subject)
          :message-id message-id
          :time (hacrm/utils:parse-time time)
          :text text)))


(defun fetch-messages-from (&key host port username password)
  (let* ((imap (apply
                #'mel.folders.imap:make-imaps-folder
                :host host
                :username username
                :password password
                (when port
                  (list :port port))))
         
         (messages (mel.public:messages imap)))
    (mapcar #'fetch-message messages)))


(defun create-contact-and-feed-item (message)
  (let* ((from (getf message :from))
         (email (getf from :email))
         (name (getf from :name))
         (subject (getf from :subect))
         (message-id (getf from :message-id))
         (time (getf from :time))
         (text (getf from :text)))
    (list email name text)))


(defun process-messages ()
  "This function fetches new emails, creates contacts and feed items."
  (let* ((accounts (ubiquitous:value :hacrm/plugins/email :accounts))
         (messages
           (loop for account in accounts
                 append (apply #'fetch-messages-from account))))
    (unless accounts
      (error "No accounts"))
    (loop for message in messages
          collect (create-contact-and-feed-item message))))
