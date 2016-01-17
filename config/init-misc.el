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

;; so sentence forward and backward don't rely on double spaces
(setq sentence-end-double-space nil)

;; window switching
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; simpler navigation
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i") 'previous-line)

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

;; smart/power line
(setq sml/no-confirm-load-theme t)
;;(setq sml/theme (quote smart-mode-line-light-powerline))
(setq sml/theme 'light)
(sml/setup)

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; weather
(require 'sunshine)
(setq sunshine-location "06830,USA")
(setq sunshine-appid "e7968552315ea82f58daf2e1ff9982b9")
(setq sunshine-show-icons t)
(setq sunshine-units (quote metric))

;; google scholar
(setq gscholar-bibtex-default-source "Google Scholar")

;; google searches
(google-this-mode 1)
(global-set-key (kbd "C-x C-g") 'google-this-lucky-search)
(setq google-this-modeline-indicator " goog")

;; short yes or no query
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove DOS line endings
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'emacs-lisp-mode-hook 'remove-dos-eol)

;; workgroups
(require 'workgroups2)
(setq wg-emacs-exit-save-behavior 'ask)
(setq wg-prefix-key (kbd "C-z"))
(setq wg-session-load-on-start nil)
(setq wg-load-last-workgroup t)
(workgroups-mode 1)
(define-key workgroups-mode-map (kbd "C-z C-z") 'wg-reload-session)
