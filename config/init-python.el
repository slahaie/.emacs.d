;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; jedi settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'jedi)
;; ;; if you need to change your python intepreter, if you want to change it
;; ;; (setq jedi:server-command
;; ;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (jedi:setup)
;; 	    ;;(jedi:ac-setup)
;; 	    (local-set-key (kbd "M-.") 'jedi:goto-definition)
;; 	    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
;; 	    (local-set-key (kbd "M-?") 'jedi:show-doc)
;; 	    (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))

;; (setq jedi:complete-on-dot t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ipython settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(elpy-enable)

(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

(setq python-shell-interpreter "C:\\python27\\python.exe"
      python-shell-interpreter-args
      "-i C:\\python27\\Scripts\\ipython-script.py console --pylab=qt")

;; (setq py-paragraph-fill-docstring-p t)
;; (setq py-set-fill-column-p t)
