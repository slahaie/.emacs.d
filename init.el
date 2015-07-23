;; Requisites: Emacs >= 24
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(package-refresh-contents)

(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

;; make more packages available with the package installer
(setq to-install
      '(python-mode autopair auto-complete flycheck jedi fill-column-indicator smooth-scrolling flx-ido projectile workgroups ipython ein))
(mapc 'install-if-needed to-install)

(require 'autopair)
(require 'auto-complete)
(require 'flycheck)
(require 'fill-column-indicator)

;; flx ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; projectile
(require 'projectile)
;;(projectile-global-mode)
(setq projectile-indexing-method 'native)

;; workgroups
(require 'workgroups)
(workgroups-mode 1)
;;(wg-load "/path/to/saved/workgroups")
(setq wg-morph-on nil)

;;;; Python-mode
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'projectile-on)
;;(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'python-mode-hook 'fci-mode)
(setq fci-rule-width 1)
(setq fci-rule-column 80)
;;(setq fci-rule-color "darkblue")

(setq py-shell-switch-buffers-on-execute-p nil)
(setq py-switch-buffers-on-execute-p nil)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automatically figure out indentation
;;(setq py-smart-indentation t)

;;;; Ipython notebook
(require 'ein)
(setq ein:use-auto-complete t)

;;;; Jedi settings
(require 'jedi)
;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))
(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    ;;(jedi:ac-setup)
	    (local-set-key (kbd "M-.") 'jedi:goto-definition)
	    (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
	    (local-set-key (kbd "M-?") 'jedi:show-doc)
	    (local-set-key (kbd "M-/") 'jedi:get-in-function-call)))

(setq jedi:complete-on-dot t)

;;;; Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(defun proj ()
  (interactive)
  (find-file "/cygdrive/c/Users/slahaie/Projects/projects.org"))


;;;;
;;;; cygwin support
;;;;
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
	     (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name) 
    (setq explicit-shell-file-name shell-file-name) 
    
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))


;;-------------------- extra things --------------------

;; window switching
(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

;; do not show ^M in files containing mixed UNIX and DOS line endings
(defun remove-dos-eol ()
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'python-mode-hook 'remove-dos-eol)

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

;;-------------------- custom settings --------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (adwaita)))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "/cygdrive/c/Users/slahaie/Projects/projects.org")
 '(initial-frame-alist (quote ((vertical-scroll-bars) (fullscreen . maximized))))
 '(initial-scratch-message "")
 '(py-paragraph-fill-docstring-p t)
 '(py-set-fill-column-p t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
