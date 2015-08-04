(elpy-enable)

(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")

(setq python-shell-interpreter "C:\\python27\\python.exe"
      python-shell-interpreter-args
      "-i C:\\python27\\Scripts\\ipython-script.py console --pylab=qt")

(eval-after-load 'elpy
  '(progn
     (define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)))

;; (setq py-paragraph-fill-docstring-p t)
;; (setq py-set-fill-column-p t)
