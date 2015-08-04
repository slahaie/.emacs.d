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
      '(python-mode autopair auto-complete flycheck jedi fill-column-indicator smooth-scrolling flx-ido projectile workgroups ipython ein heroku-theme tango-plus-theme stekene-theme auctex ess))
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

;;;; ESS (Emacs Speaks Statistics)
(require 'ess)

;;;; AUCTeX

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;;(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; use pdflatex
(setq TeX-PDF-mode t) 
 
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill) 
(add-hook 'LaTeX-mode-hook (lambda () (abbrev-mode +1))) 
(server-start)

;; FROM: http://www.emacswiki.org/emacs/TN
(require 'tex-buf)
(defun TeX-command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
  (cond ((if (string-equal name TeX-region)
             (TeX-check-files (concat name "." (TeX-output-extension))
                              (list name)
                              TeX-file-extensions)
           (TeX-save-document (TeX-master-file)))
         TeX-command-default)
        ((and (memq major-mode '(doctex-mode latex-mode))
              (TeX-check-files (concat name ".bbl")
                               (mapcar 'car
                                       (LaTeX-bibliography-list))
                               BibTeX-file-extensions))
         ;; We should check for bst files here as well.
         TeX-command-BibTeX)
        ((TeX-process-get-variable name
                                   'TeX-command-next
                                   TeX-command-Show))
        (TeX-command-Show)))

(defcustom TeX-texify-Show t
  "Start view-command at end of TeX-texify?"
  :type 'boolean
  :group 'TeX-command)

(defcustom TeX-texify-max-runs-same-command 5
  "Maximal run number of the same command"
  :type 'integer
  :group 'TeX-command)

(defun TeX-texify-sentinel (&optional proc sentinel)
  "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
If there is still something left do do start the next latex-command."
  (set-buffer (process-buffer proc))
  (funcall TeX-texify-sentinel proc sentinel)
  (let ((case-fold-search nil))
    (when (string-match "\\(finished\\|exited\\)" sentinel)
      (set-buffer TeX-command-buffer)
      (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
        (TeX-texify)))))

(defun TeX-texify ()
  "Get everything done."
  (interactive)
  (let ((nextCmd (TeX-command-default (TeX-master-file)))
        proc)
    (if (and (null TeX-texify-Show)
             (equal nextCmd TeX-command-Show))
        (when  (called-interactively-p 'any)
          (message "TeX-texify: Nothing to be done."))
      (TeX-command nextCmd 'TeX-master-file)
      (when (or (called-interactively-p 'any)
                (null (boundp 'TeX-texify-count-same-command))
                (null (boundp 'TeX-texify-last-command))
                (null (equal nextCmd TeX-texify-last-command)))
        (mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
        (setq TeX-texify-count-same-command 1))
      (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
          (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
        (setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
        (setq TeX-texify-last-command nextCmd)
        (and (null (equal nextCmd TeX-command-Show))
             (setq proc (get-buffer-process (current-buffer)))
             (setq TeX-texify-sentinel (process-sentinel proc))
             (set-process-sentinel proc 'TeX-texify-sentinel))))))

; don't run View after compiling
(setq TeX-texify-Show nil)

; key hook for compiling (C-c C-a)
(add-hook 'LaTeX-mode-hook 
    '(lambda () (local-set-key (kbd "C-c C-a") 'TeX-texify)))

(defun TeX-save-and-texify ()
  "Save and then run texify."
  (interactive)
  (save-buffer)
  (TeX-texify))

; NOTE: place this AFTER all AUCTeX key bindings are complete
;
; key hook for save and compile (replaces C-x C-s)
; this hooks when opened file has extension .tex
; (b/c hooking on 'LaTeX-mode-hook will cause .cls files to be compiled too)
(add-hook 'find-file-hook 
    ; only execute when file extension matches .tex
    '(lambda() (when (and (stringp buffer-file-name)
                          (string-match "\\.tex\\'" buffer-file-name))
         ; make buffer-local copy of local-key-map
         ; this is b/c local key maps are for entire major modes
         (use-local-map (copy-keymap LaTeX-mode-map))
         (local-set-key (kbd "C-x C-s") 'TeX-save-and-texify))))


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
    (setenv "HOME" (concat cygwin-root "/home/slahaie"))
    
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

;; color theme
(load-theme 'tango-plus t)
;;(load-theme 'heroku t)
;;(load-theme 'stekene-light t)

;;-------------------- custom settings --------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Sumatra PDF"
      ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o")
      nil))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Sumatra PDF")
     (output-html "xdg-open"))))
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("bac3f5378bc938e96315059cd0488d6ef7a365bae73dac2ff6698960df90552d" "2916d16e583c17bb2a1a9d231ea8ddcb3577f8cb97179eea689e91036213ff03" "95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" default)))
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers)
     (ess-fl-keyword:operators)
     (ess-fl-keyword:delimiters)
     (ess-fl-keyword:=)
     (ess-R-fl-keyword:F&T)
     (ess-R-fl-keyword:%op%))))
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
