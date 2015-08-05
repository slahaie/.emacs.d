;;;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun proj ()
  (interactive)
  (find-file "C:/Users/Sebastien/Projects/projects.org"))
