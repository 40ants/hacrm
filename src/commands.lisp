(defpackage #:hacrm.commands
  (:use #:cl)
  (:export
   #:command
   #:process-query
   #:default-command))
(in-package hacrm.commands)


(defgeneric default-command (widget)
  (:documentation "Returns a keyword for default command for a widget.

This method is used when no applicable method was found for user's input.

It should return a keyword, denoting a command's name."))


(defmethod default-command ((widget t))
  "By default, we'll try to search something for every widget."
  :search)


(defgeneric command (widget keyword rest-text)
  (:documentation "Processes a command with given name when a widget is active.

This command can be used to emulate Emacs's mode in HaCRM.

It is called with current main widget and a symbol user entered in the input box.

First user's toke is considered as a command name and transformed to a keyword.

If no method was defined to handle a keyword, then another call to `command' will
be made but with (hacrm.widgets.base:default-command widget) in place of the keyword."))


(define-condition no-command-error ()
  ())


(defmethod no-applicable-method ((generic-function (eql #'command))
                                 &rest function-arguments)
  (declare (ignorable generic-function
                      function-arguments))
  (error 'no-command-error))


(defun process-query (widget text)
  "Parses text as a lisp's form and calls a command with given name.

It consider any text a form, by adding an implicit braces around it.

If first item of a list is a symbol, then it is converted to a keyword
and then generic function hacrm.command:command is called."
  (let* ((first-token (first (cl-strings:split text #\Space)))
         (rest-text (subseq text (min (length first-token)
                                      (+ (length first-token)
                                         1))))
         (keyword (intern (string-upcase first-token)
                          :keyword)))
    (log:debug "Processing query" text)
    
    (handler-case (command widget keyword rest-text)
      (no-command-error ()
        (log:warn "No command" keyword)
        (command widget
                 (default-command widget)
                 text)))))

