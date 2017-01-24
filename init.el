;; Requisites: Emacs >= 24
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives
;;	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(elpy
	autopair
	auto-complete
	flycheck
	py-autopep8
	jedi
	fill-column-indicator
	smooth-scrolling
	flx-ido
	workgroups2
	projectile
	magit
	ein
	markdown-mode
	smart-mode-line
	auctex
	auctex-latexmk
	gscholar-bibtex
	google-this
	sunshine
	tango-plus-theme
	solarized-theme
	zenburn-theme
	heroku-theme))
(mapc 'install-if-needed to-install)


(add-to-list 'load-path "~/.emacs.d/config/")
(load "init-misc")
(load "init-org")
;;(load "init-latex")
;;(load "init-python")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(initial-scratch-message nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sunshine-forecast-date-face ((t (:foreground "PeachPuff4" :weight ultra-bold))))
 '(sunshine-forecast-headline-face ((t (:foreground "PeachPuff4" :height 1.5)))))
