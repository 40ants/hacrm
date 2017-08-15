;; (require 'slime)

;; (slime-setup '(slime-company slime-fancy))
;; ;;(slime)

;; (eval-after-load 'company              
;;   '(progn                               
;;      (define-key company-mode-map (kbd "M-/") 'helm-company)
;;      (define-key company-active-map (kbd "M-/") 'helm-company)))


(add-hook 'slime-repl-mode-hook
          (lambda ()
            (slime-eval '(cl:progn
                          (cl:pushnew "./" asdf:*central-registry*)
                          (cl:declaim (cl:optimize (cl:debug 3)))
                          (ql:quickload 'hacrm)))))

(slime)

(load "~/.roswell/lisp/quicklisp/log4slime-setup.el")
(global-log4slime-mode 1)
