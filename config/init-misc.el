;; initial buffer
;;(setq initial-buffer-choice "~/Projects/projects.org")

;; layout definition
(defun my-startup-layout ()
 (interactive)
 (delete-other-windows)
 (split-window-horizontally) ;; -> |
 (next-multiframe-window)
 (find-file "~/Projects/projects.org")
 (next-multiframe-window)
 (find-file "~/Projects/gtd.org")
 (next-multiframe-window)
)
;; execute the layout
(my-startup-layout)

;; flx ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; workgroups
(require 'workgroups)
(workgroups-mode 1)
(setq wg-switch-on-load nil)
(wg-load "~/.emacs.d/workgroups")
(setq wg-morph-on nil)

;; so sentence forward and backward don't rely on double spaces
(setq sentence-end-double-space nil)

;; window switching
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

;; magit status buffer
(global-set-key (kbd "C-x g") 'magit-status)

;; smooth scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;; don't create backup files
(setq make-backup-files nil)

;; rebind word deletion key
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; to call keyboard macros
(global-set-key [f5] 'call-last-kbd-macro)

;; rebind backspace key
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key [f6] 'help-command)

;; color theme
(load-theme 'tango-plus t)
;(load-theme 'heroku t)
;(load-theme 'stekene-light t)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

