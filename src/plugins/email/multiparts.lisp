;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2010, Fred Gibson <fred@streamfocus.com>.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage :com.streamfocus.mel.mime
  (:use :cl)
  (:export
   "SAVE-ATTACHMENT-FILE"
   "EML-MESSAGE->VIEWABLE-PART"
   "EML->SUBJECT"
   "EML-ADDRESS->NAME"
   "EML-ADDRESS->EMAIL"
   "EML-FIND-ATTACHMENTS"
   "EML-ATTACHMENT-NAME"
   "EML-PART->PARENT"
   "EML-CONTENT-CHARSET"
   "PART-HTML-PAGE?"
   "PART-BODY-HTML?"
   "PART-BODY-HTML"
   "PART-BODY-TEXT?"
   "PART-BODY-TEXT"
   "EML-DECODE-RFC2822"
   )
  (:nicknames :sf.mel))

(in-package :com.streamfocus.mel.mime)

(defun save-attachment-file (part filename)
 (let* ((attach (mel.mime:part-body-string part))
        (length (length
                 (string-trim '(#\newline #\return #\linefeed)
                              (read-line (make-string-input-stream attach)))))
        (in (make-string-input-stream attach)))
   (with-open-file (out filename :direction :output :element-type '(unsigned-byte 8))
     (loop with buffer = (make-array length :element-type 'character)
        for count = (read-sequence buffer in)
        while (> count 0)
        do (write-sequence (mel.mime::decode-base64 buffer) out)
          (flet ((peek ()(peek-char nil in nil :eof))
                 (consume () (read-char in nil :eof)))
            (tagbody
             start (let ((c (peek)))
                     (when (member c '(#\return #\linefeed #\newline))
                       (progn (consume) (go start))))))))))

;;TODO -> find/save/view alternative/related files see 10569-mail.eml for format

(defun eml-content-type (obj)
          (multiple-value-bind (a b c)(mel.mime:content-type obj)
            (declare (ignore b c))
            a))

(defun eml-message->viewable-part (obj)
 "takes a part or eml message"
 (flet ((eml-content-subtype (o)
          (multiple-value-bind (a b c)(mel.mime:content-type o)
            (declare (ignore a c))
            b))
        )
   (if (eq (eml-content-subtype obj) :plain)
       obj
       (let ((parts (mel.mime:parts obj)))
         (when parts
           (or
            (find :plain parts :key #'eml-content-subtype)
            (let ((mpart (find :multipart parts :key #'eml-content-type)))
              (when mpart (eml-message->viewable-part mpart)))))))))

(defun eml-address->email (eml-address)
  (when (slot-boundp eml-address 'mel:address-spec)
    (mel:address-spec eml-address)))

(defun eml-address->name (eml-address)
  (when (slot-boundp eml-address 'mel:display-name)
    (decode-string 
     (string-right-trim
      " "
      (string-left-trim 
       " "
       (remove #\" (mel:display-name eml-address)))))))

(defun eml-part-base64? (part)
  (when (eq (mel.mime:content-transfer-encoding part) :base64)
    t))

(defun eml-find-attachments (eml)
  (flet ((eml-mixed? ()
           (multiple-value-bind (a b c) (mel.mime:content-type eml)
             (declare (ignore a c))
             (when (eq b :mixed) t))))
    (when (eml-mixed?)
      (let (attach)
        (dolist (part (mel:parts eml))
          (when (eml-part-base64? part)
            (push part attach)))
        attach))))

(defun eml-attachment-name (part)
  "Returns attachment filename string"
  (when (eml-part-base64? part)
    (getf (mel.mime::content-parameters part) :name)))
    
(defun eml-part->parent (part)
  (if (typep part 'mel.public:mime-message)
      part
      (let ((parent (mel.mime::parent part)))
        (if parent
            (eml-part->parent parent)
            part))))

(defun eml-part-text? (part)
  (eq (eml-content-subtype part) :plain))

(defun eml-part-html? (part)
  (eq (eml-content-subtype part) :html))

(defun eml-content-subtype (obj)
  (multiple-value-bind (a b c)(mel.mime:content-type obj)
             (declare (ignore a c))
             b))

(defun eml-content-charset (obj)
  (multiple-value-bind (a b c)(mel.mime:content-type obj)
             (declare (ignore a b))
             (getf c :charset)))

(defun eml-message->text-part (obj)
  "takes a part or eml message"
  (if (and (typep obj 'mel.mime:part)
           (eml-part-text? obj))
      obj
      (let ((parts (mel.mime:parts obj)))
        (when parts
          (or
           (find :plain parts :key #'eml-content-subtype)
           (let ((mpart (find :multipart parts :key #'eml-content-type)))
             (when mpart (eml-message->text-part mpart))))))))

(defun eml-message->html-part (obj)
  "takes a part or eml message"
  (if (and (typep obj 'mel.mime:part)
           (eml-part-html? obj))
      obj
      (let ((parts (mel.mime:parts obj)))
        (when parts
          (or
           (find :html parts :key #'eml-content-subtype)
           (let ((mpart (find :multipart parts :key #'eml-content-type)))
             (when mpart (eml-message->html-part mpart))))))))

(defun part-html-page? (msg)
  (declare (optimize (speed 2)))
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg)))
         (stream (cond (part
                        (mel.mime:part-body-stream part))
                       (msg-html?
                        (mel.public:message-body-stream msg))))
         (match 0))
    (declare (type fixnum match))
    (when stream
      (labels ((peek ()
                 (peek-char nil stream nil :eof))
               (ignore ()
                 (read-char stream nil :eof))
               (try ()
                 (read-char stream nil :eof)
                 (incf match))
               (match? (code)
                 (declare (type string code))
                 (if (= match (length code))
                     t
                     (let ((c (peek)))
                       (declare (type character c))
                       (cond ((char= c (char code match))
                              (try)(match? code))
                             ((eq c :eof) nil)
                             (t nil)))))
               (match-multi? (codes)
                 (if (car (remove-if #'null 
                                     (mapcar (lambda (x)
                                               (declare (type string x))
                                               (= match (length x))) 
                                             codes)))
                     t
                     (let ((c (peek)))
                       (declare (type character c))
                       (cond ((member c (mapcar 
                                       (lambda (x)
                                         (declare (type string x))
                                         (char x match)) codes))
                              (try)(match-multi? codes))
                             ((eq c :eof) nil)
                             (t nil)))))
               (reset ()
                 (setf match 0)
                 (start))
               (code? ()
                 (try)
                 (let ((c (peek)))
                   (case c
                     ((#\Newline #\return #\linefeed)
                      (ignore)
                      (let ((c (peek)))
                        (case c
                          ((#\Newline #\return #\linefeed)(ignore)(reset))
                          (t (reset)))))
                     (#\! (match-multi? '("<!DOCTYPE" "<!doctype")))
                     (#\h (match-multi? '("<html" "<head")))
                     (#\H (match? "<HTML")) 
                     (#\s (match? "<style"))
                     (#\S (match? "<STYLE"))
                     (#\> nil)
                     (:eof nil)
                     (t nil))))
               (start ()
                 (let ((c (peek)))
                   (case c
                     (#\< (code?))
                     (:eof nil)
                     (t (ignore)
                        (start)))))
               )
      (when msg-html?
        (mel.mime:skip-rfc2822-header stream))
      (start)))))

(defun part-body-html? (msg)
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg))))
    (part-body-probe :part part :msg (when msg-html? msg))))

(defun part-body-text? (msg)
  (let* ((msg-text? (eml-part-text? msg))
         (part (unless msg-text?
                 (eml-message->text-part msg))))
    (part-body-probe :part part :msg (when msg-text? msg))))
         
(defun part-body-probe (&key part msg)
  (let ((stream (cond (part
                       (mel.mime:part-body-stream part))
                      (msg
                       (mel.public:message-body-stream msg)))))
    (when stream
      (when (read-char stream nil nil)
        (close stream)
        t))))

(defun base64->string (string charset)
  (flexi-streams:octets-to-string (mel.mime::decode-base64 string)
   :external-format (or (keyword-symbol charset) :utf-8)))

(defun part-body-decode (&key part msg string-fn)
  (let ((string (cond (part
                        (mel.mime:part-body-string part))
                       (msg
                        (message-body-string msg))))
        (charset (cond (part
                        (eml-content-charset part))
                       (msg
                        (eml-content-charset msg))))
        (transfer-encoding (mel.mime:content-transfer-encoding (or part msg))))
    (when string
      (if (eq transfer-encoding :base64)
          (progn
            (setf string (base64->string (remove #\return (remove #\linefeed string))
                                         charset))
            (if string-fn
                (funcall string-fn string)
                string))
          (eml-decode-rfc2822 string :string-fn string-fn :external-format (keyword-symbol charset))))))

(defun eml->subject (msg)
  (decode-string (mel.mime:subject msg)))

(defun decode-string (string);TODO make universal the hard way
  (if (> (length string) 16)
      (cond ((member (subseq string 0 10) '("=?UTF-8?B?" "=?utf-8?B?"))
             (let (result)
               (dolist (string (split-string string #\linefeed))
                 (setf result 
                       (concatenate 
                        'string 
                        result 
                        (base64->string 
                         (string-right-trim "?="
                                            (string-left-trim 
                                             "=?utf-8?B?"
                                             (string-left-trim "=?UTF-8?B?" (remove #\space string))))
                         "utf-8"))))
               result))
            ((member (subseq string 0 10) '("=?UTF-8?Q?" "=?utf-8?Q?"))
             (let (result)
               (dolist (string (split-string string #\linefeed))
                 (setf result 
                       (concatenate 
                        'string 
                        result 
                        (eml-decode-rfc2822
                         (string-right-trim "?="
                                            (string-left-trim 
                                             "=?utf-8?Q?"
                                             (string-left-trim "=?UTF-8?Q?" (remove #\space string))))
                         ))))
               result))
            ((member (subseq string 0 15) '("=?ISO-8859-1?Q?" "=?iso-8859-1?Q?") :test #'string=)
             (let (result)
               (dolist (string (split-string string #\linefeed))
                 (setf result 
                       (concatenate 
                        'string 
                        result 
                        (eml-decode-rfc2822
                         (string-right-trim "?="
                                            (string-left-trim 
                                            "=?iso-8859-1?Q?"
                                             (string-left-trim "=?ISO-8859-1?Q?" (remove #\space string))))
                         :external-format :ISO-8859-1))))
               result))
            ((member (subseq string 0 15) '("=?ISO-8859-1?B?" "=?iso-8859-1?B?") :test #'string=)
             (let (result)
               (dolist (string (split-string string #\linefeed))
                 (setf result 
                       (concatenate 
                        'string 
                        result 
                        (base64->string 
                         (subseq 
                          (string-left-trim 
                           "=?ISO-8859-1?B?" 
                           (string-left-trim
                            "=?iso-8859-1?B?"
                           (remove #\space string)))
                          0 (- (length string) 17))
                         "ISO-8859-1"))))
               result))
            (t string))
      string))

(defun part-body-html (msg &key string-fn)
  (let* ((msg-html? (eml-part-html? msg))
         (part (unless msg-html?
                 (eml-message->html-part msg))))
    (part-body-decode :part part :msg (when msg-html? msg) :string-fn string-fn)))
    
(defun part-body-text (msg &key string-fn)
  (let* ((msg-text? (eml-part-text? msg))
         (part (unless msg-text?
                 (eml-message->text-part msg))))
    (part-body-decode :part part :msg (when msg-text? msg) :string-fn string-fn)))

(defun message-body-string (msg)
  (with-output-to-string (out)
    (with-open-stream (s (mel.public:message-body-stream msg))
      (do ((c (read-char s nil :eof)(read-char s nil :eof)))
          ((eq c :eof) out)
        (case c
          (otherwise (write-char c out)))))))

(defun eml-decode-rfc2822 (string &key string-fn (external-format :utf-8))
  (declare (type string string) (optimize (speed 2)))
  (let ((in (make-string-input-stream string))
        (out (make-string-output-stream))
        code-match
        code-buffer
        buffer)
    (labels ((peek ()
               (peek-char nil in nil :eof))
             (consume ()
               (let ((char (read-char in nil :eof)))
                 (output (string char))))
             (output (string)
               (if string-fn
                   (funcall string-fn string)
                   (princ string out)))
             (output-code ()
               (when code-match
                 (handler-case
                     (output (flexi-streams:octets-to-string (hex-string-to-byte-array code-match)
                                               :external-format external-format))
                   (t () (output code-match)))
                 (setf code-match nil)))
             (ignore ()
               (read-char in nil :eof))
             (try ()
               (setf buffer (concatenate 'string buffer (string (read-char in nil :eof))))
               )
             (match? (&key equal)
               (when equal (try))
               (dotimes (c 2)
                 (let ((h (read-char in nil :eof)))
                   (case h
                     (:eof 
                      (output-code)
                      (return-result)
                      (when code-buffer
                        (output code-buffer)))
                     (t
                      (setf code-buffer (concatenate 'string code-buffer (string h)))))))
               (if (hexidecimal? code-buffer)
                   (progn
                     (setf code-match (concatenate 'string code-match code-buffer))
                     (setf code-buffer nil buffer nil)
                     (match? :equal t))
                   (progn
                     (output-code)
                     (when buffer (output buffer))
                     (when code-buffer
                       (output code-buffer))
                     (reset)
                     )))
             (reset ()
               (setf buffer nil code-buffer nil)
               (start))
             (code? ()
               (try)
               (let ((c (peek)))
                 (cond
                   ((member c '(#\Newline #\return #\linefeed))
                    (ignore)
                    (let ((c (peek)))
                      (case c
                        ((#\Newline #\return #\linefeed)(ignore)(reset))
                        (t (reset)))))
                   ((eq c :eof) (output-code)(return-result))
                   ((hexidecimal? (string c) :digits 1)
                    (match?)
                    )
                   (t 
                    (output-code)
                    (output buffer)
                      (reset)))))
             (start ()
               (let ((c (peek)))
                 (case c
                   (#\= (code?))
                   (:eof (return-result))
                   (t (consume)
                      (start)))
                 ))
             (return-result ()    
               (when buffer
                 (output buffer)))
               )
      (start)
      (unless string-fn
        (get-output-stream-string out)))))

(defvar *hexidecimal*  (vector '#\0 '#\1 '#\2 '#\3 '#\4 '#\5 '#\6 '#\7 '#\8 '#\9 '#\a '#\A '#\b '#\B '#\c '#\C '#\d '#\D '#\e '#\E '#\f '#\F))

(defvar *hexidecimal-CAPS*  (vector '#\0 '#\1 '#\2 '#\3 '#\4 '#\5 '#\6 '#\7 '#\8 '#\9 '#\A '#\B '#\C '#\D '#\E '#\F))

(defun hexidecimal? (string &key (digits 2) (hex-caps t))
  (let ((in (make-string-input-stream string))
        (bytes (/ (length string) digits))
        (hex-vector (if hex-caps
                        *hexidecimal-CAPS*
                        *hexidecimal*))
        )
    (when (integerp bytes)
      (labels ((peek ()
                 (peek-char nil in nil :eof))
               (consume ()
                 (read-char in nil :eof))
               (hexi? ()
                 (let ((c (peek)))
                   (cond
                     ((eq c :eof) t)
                     ((find c hex-vector)
                      (consume)
                      (hexi?))
                     (t nil)))))
          (hexi?)))))

;;from ironclad testfuns.lisp
(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  ;; This function disappears from profiles if SBCL can inline the
  ;; POSITION call, so declare SPEED high enough to trigger that.
  (declare (type string string)
           (optimize (speed 2))
           )
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key)
             (type fixnum end start))
    (flet ((char-to-digit (char)
             (let ((x (cl:position char "0123456789abcdef"
                                   :test #'char-equal)))
               (or x (error "Invalid hex key ~A specified" string)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
            finally (return key)))))

(defmethod keyword-symbol ((name string))
  (intern 
   #-allegro (string-upcase name) 
   #+allegro (string-downcase name)
   'keyword))

(defun split-string (string char)
  "return a list of substrings which are separated by the char in the input string"
  (if string
      (let ((p (position char string)))
        (if p
            (cons (subseq string 0 p)
                  (if (< p (length string))
                      (split-string (subseq string (+ 1 p)) char)
                      nil))
            (cons string nil)))))