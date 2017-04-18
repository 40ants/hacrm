
(in-package :hacrm)


(defwidget list-dependencies ()
  ())


(defmethod render-widget-body ((widget list-dependencies) &rest args)
  (declare (ignorable args))
  (with-html
    (:ul :class "page-dependencies"
         (loop :for item :in (append (webapp-application-dependencies)
                                     weblocks::*page-dependencies*)
               :do (htm
                    (:li (esc (format nil "dep: ~s" item))))
               ))))

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
        (list (lambda (&rest args)
                (declare (ignorable args))
                (with-html
                  (:div :class "container"
                        (:h1 :align "center" "Happy Hacking!"))))
              (make-instance 'list-dependencies))))

