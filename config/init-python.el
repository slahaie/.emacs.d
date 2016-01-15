;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elpy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(elpy-enable)
(elpy-use-ipython)
(setq
 python-shell-interpreter "C:\\Python27\\python.exe"
 python-shell-interpreter-args
 "-i C:\\Python27\\Scripts\\ipython-script.py")
(setq elpy-rpc-backend "jedi")
(setq elpy-test-runner 'elpy-test-pytest-runner)
(eval-after-load 'elpy
  '(progn
     (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; emacs iptyhon notebook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ein)
(setq ein:use-auto-complete t)
(setq ein:complete-on-dot nil)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
