(defpackage #:hacrm/widgets/version
  (:use #:cl)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:hacrm/desktop)
  (:import-from #:hacrm/widgets/base
                #:base)
  (:import-from #:cl-info
                #:make-cl-info)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:hacrm/utils
                #:get-machine-name)
  (:export
   #:make-version-widget))
(in-package hacrm/widgets/version)


(defwidget version (base)
  ())


(defun make-version-widget ()
  "Creates a widget to information about current HACRM's version."
  (make-instance 'version))


(defmethod render ((widget version))
  (let ((debug-mode (weblocks/debug:status))
        (version (asdf:component-version (asdf:find-system :hacrm)))
        (system-info (make-cl-info)))
    
    (with-html
      (:h1 "Version info")
      (:pre (with-output-to-string (s)
              (format s "HACRM: ~A~%" version)
              (format s "Machine: ~A~%" (get-machine-name))
              (princ system-info s))))

    (when debug-mode
      (with-html
        (:pre (format nil "Debug mode is ON
    HTTP port: ~A
    Slynk port: ~A"
                      hacrm/desktop::*port*
                      hacrm/debug::*slynk-port*))))))
