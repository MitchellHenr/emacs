;;; package --- Init file
;;; Commentary:
;;; This be me Emacs init file
;;; Code:
(server-start)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(exec-path-from-shell-initialize)
(eval-when-compile
  (require 'use-package))
(defconst user-init-directory
  "~/.emacs.d/config")

(defun load-user-file (file)
  "Load a file FILE in current user's configuration directory."
  (interactive "File to load: ")
  (load-file (expand-file-name file user-init-directory)))

(load-user-file "appearance-config.el")
(load-user-file "evil-config.el")
(load-user-file "editing-config.el")

;; ABBREVS
(setq-default abbrev-mode t)
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")

;; MODES
(global-flycheck-mode 1)
(electric-pair-mode 1)
(ivy-mode 1)
(which-key-mode 1)
(show-paren-mode 1)
(hl-line-mode 1)

;; AUTOCOMPLETION

;; GENERAL HOOKS
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; AUCTEX
(setq-default TeX-master nil) ; Tries to find a master file.
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (TeX-fold-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda ()
			     (latex-math-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda ()
			     (prettify-symbols-mode 1)))
(add-hook 'LaTeX-mode-hook (lambda ()
			     (outline-minor-mode 1)))
;; ORG
(require 'org-agenda)
(define-key org-agenda-mode-map "t" 'butterfly)
(define-key org-agenda-mode-map "j" 'evil-next-line)
(define-key org-agenda-mode-map "k" 'evil-previous-line)

;; RUBY
(add-hook 'ruby-mode-hook
	  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
		`(ruby-mode
		  ,(rx (or "def" "class" "if" "while" "else" "module" "do" "{" "[")) ; Block start
		  ,(rx (or "}" "]" "else" "end"))                       ; Block end
		  ,(rx (or "#" "=begin"))                        ; Comment start
		  ruby-forward-sexp nil)))

;; BACKUPS
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   (quote
    ((61 "implies" "" nil)
     (56 "infty" "" nil)
     (101 "varepsilon" "" nil))))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(custom-safe-themes
   (quote
    ("1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" default)))
 '(evil-want-Y-yank-to-eol t)
 '(global-prettify-symbols-mode t)
 '(linum-relative-current-symbol "->")
 '(org-agenda-files
   (quote
    ("~/Todo/school/EE004.org" "~/Todo/school/EE082.org" "~/Todo/school/PHYS214.org" "~/Todo/school/MATH052.org" "~/Todo/school/MATH235.org" "~/Todo/school/HON101.org" "~/Todo/school/EE003.org" "~/Todo/school/EE081.org" "~/Todo/school/EE131.org" "~/Todo/school/MATH330.org" "~/Todo/school/PHYS213.org" "~/Todo/school/research.org" "~/Todo/school/misc.org" "~/Todo/life/list.org")))
 '(package-selected-packages
   (quote
    (ample-zen-theme markdown-mode+ markdown-mode vi-tilde-fringe auto-complete go-mode key-chord neotree paganini-theme auto-yasnippet which-key syndicate popup org-evil monokai-theme julia-mode ivy flycheck exec-path-from-shell evil-visual-mark-mode evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-leader evil-goggles evil-easymotion evil-commentary)))
 '(require-final-newline nil)
 '(ring-bell-function (quote ignore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
;; (setq mode-line-format
;;        (list "-"
;;         'mode-line-mule-info
;;         'mode-line-modified
;;         'mode-line-frame-identification
;;         "%b--"
;;         ;; Note that this is evaluated while making the list.
;;         ;; It makes a mode line construct which is just a string.
;;         (getenv "HOST")
;;         ":"
;;         'default-directory
;;         "   "
;;         'global-mode-string
;;         "   %[("
;;         '(:eval (mode-line-mode-name))
;;         'mode-line-process
;;         'minor-mode-alist
;;         "%n"
;;         ")%]--"
;;         '(which-func-mode ("" which-func-format "--"))
;;         '(line-number-mode "L%l--")
;;         '(column-number-mode "C%c--")
;;         '(-3 "%p")))
;; (setq-default evil-mode-line-format
;; 	      '(" %b"
;; 		" [%*]"
;; 		" col: %c"
;; 		" (%l/%p)"))
