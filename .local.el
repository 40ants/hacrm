;; (require 'slime)

;; (slime-setup '(slime-company slime-fancy))
;; ;;(slime)

;; (eval-after-load 'company              
;;   '(progn                               
;;      (define-key company-mode-map (kbd "M-/") 'helm-company)
;;      (define-key company-active-map (kbd "M-/") 'helm-company)))


(add-hook 'slime-repl-mode-hook
          (lambda ()
            (slime-eval '(ql:quickload 'hacrm)))
          ;; append
          t)

(add-hook 'lisp-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (hs-hide-all)
            
            (local-set-key (kbd "C-c <up>") 'hs-hide-block)
            (local-set-key (kbd "C-c C-x <up>") 'hs-hide-all)
            (local-set-key (kbd "C-c <left>") 'bm-previous)
            (local-set-key (kbd "C-c <down>") 'hs-show-block)
            (local-set-key (kbd "C-c <right>") 'bm-next))
          ;; append
          t)

(slime)

;; (load "~/.roswell/lisp/quicklisp/log4slime-setup.el")
;; (global-log4slime-mode 1)

