(defpackage #:hacrm.widgets.contacts-list
  (:use #:cl
        #:weblocks
        #:hacrm.models.contact
        #:f-underscore)
  (:import-from #:cl-who
                #:esc)
  (:import-from #:parenscript
                #:@)
  (:import-from #:weblocks.ui.form
                #:render-link)
  (:export
   #:make-contacts-list
   #:contacts-list
   #:contacts
   #:render-facts
   #:show-fact-group-in-contact-list-p))
(in-package hacrm.widgets.contacts-list)


(defwidget contact-card (hacrm.widgets.base:base)
  ((contact :type 'contact
            :initarg :contact
            :reader contact)
   (fact-groups :initarg :fact-groups
                :reader fact-groups)
   (on-click :type function
             :initarg :on-click
             :reader on-click)
   (number :type integer
           :initarg :number
           :reader number)))


(defwidget contacts-list (hacrm.widgets.base:base)
  ((contacts :initarg :contacts
             :reader contacts)))


(defgeneric show-fact-group-in-contact-list-p (fact-group)
  (:documentation "Accepts a keyword denoting a fact group returns nil or t.

If nil is returned, then this kind of facts does not rendered in contact list
mode.

By default, nil is returned. If you really need to render the facts
in contact list mode, redefine this method.")
  (:method (fact-group)
    (declare (ignorable fact-group))
    nil))


(defun make-contact-card (contact on-click number)
  (let* ((all-fact-groups (hacrm.models.facts.core:fact-groups
                       contact))
         (fact-groups (remove-if-not #'show-fact-group-in-contact-list-p
                                     all-fact-groups))
         (fact-group-widgets
           (mapcar
            (f_ (hacrm.widgets.facts:make-facts-group-widget
                 _
                 contact))
            fact-groups)))
    
    (make-instance 'contact-card
                   :contact contact
                   :on-click on-click
                   :number number
                   :fact-groups fact-group-widgets)))


(defun make-contacts-list (contacts &key on-contact-click)
  (flet ((default-click-processor (contact)
           (log:info "No action was passed to process selection of the"
                     contact)))
    
    (make-instance
     'contacts-list
     :contacts (loop for contact in contacts
                     for number upfrom 1
                     collect (make-contact-card
                              contact
                              (or on-contact-click
                                  #'default-click-processor)
                              number)))))


(defmethod render-widget-body ((widget contact-card) &rest rest)
  "Internal helper to render single contact in the contact's list."
  (declare (ignorable rest))
  
  (let ((contact (contact widget))
        (fact-group-widgets (fact-groups widget))
        (on-click-callback (on-click widget)))

    (with-html
      (:div :class "contact-list__contact"
            :id (format nil
                        "contact-~a"
                        (number widget))
            (:h1 (render-link (f_% (funcall on-click-callback
                                            contact))
                              (name contact))
                 (:span :class "contact-list__contact-number"
                        (esc (princ-to-string (number widget)))))

            (mapcar #'render-widget fact-group-widgets)))))


(defmethod render-widget-body ((widget contacts-list) &rest rest)
  (declare (ignorable rest))
  
  (let ((contacts (contacts widget)))

    (if contacts
        (mapcar #'render-widget
                contacts)
        
        ;; No contacts
        (with-html
          (:p "No contacts")))))


(defmethod weblocks.dependencies:get-dependencies  ((widget contacts-list))
  (list (weblocks.parenscript:make-dependency
          (let ((numbers-are-visible nil))

            ;; With this code we give user ability to press Alt+1, Alt+2,...
            ;; to select one of first 10 contacts in the list.
            (setf (@ document onkeydown)
                  (lambda (e)
                    (let ((code (@ e "keyCode"))
                          (numbers (j-query ".contact-list__contact-number")))
                      ((@ console log) "Key down with code:" code)
                    
                      (cond
                        ((= code 18)
                         ((@ numbers show))
                         (setf numbers-are-visible t))
                        ((and (>= 57 code 48))
                         ;; If user presed 0, then we'l consider it a 10.
                         (when (= code 48)
                           (setf code (+ code 10)))
                         
                         (let ((contact-number (- code 48)))
                           ((@ console log)
                            "Selecting contact" contact-number)
                           ;; jQuery("#contact-0 a").click()
                           ((@ (j-query (+ "#contact-" contact-number " a"))
                               click))))))))

            (setf (@ document onkeyup)
                  (lambda (e)
                    (let ((code (@ e "keyCode"))
                          (numbers (j-query ".contact-list__contact-number")))
                      ((@ console log) "Key up with code:" code)
                      
                      (cond
                        ((= code 18)
                         ((@ numbers hide))
                         (setf numbers-are-visible nil)))))))
          )
        (weblocks.lass:make-dependency
          '(.contact-list__contact-number
            :display none
            :position relative
            :top -0.5em
            :color gray))))
