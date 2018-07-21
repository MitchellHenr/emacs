(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Packages
(use-package tex
  :ensure auctex
  :defer 1
  :config
  (setq Tex-tree-roots '("~/.texlive2017" "~/texlive2016"))
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (setq LaTeX-math-list
	'((?, "qc" "" nil)
	  (?6 "partial" "" nil)
	  (?= "implies" "" nil)
	  (?8 "infty" "" nil)
	  (?e "varepsilon" "" nil))))

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  (setq evil-search-module 'evil-search)
  (setq evil-want-Y-yank-to-eol 1)
  :general
  ("C-k" 'evil-window-up
   "C-j" 'evil-window-down
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   "C-c h" 'help)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "h[elp]" 'help)
  (evil-ex-define-cmd "W[rite]" 'evil-save)
  (evil-ex-define-cmd "E[dit]" 'evil-edit)

  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (setq evil-vsplit-window-left 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package org
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (setq org-agenda-files
	'("~/Todo/school/"))
  :general
  ("C-c a" 'org-agenda
   "C-c c" 'org-capture)
  (:keymaps 'org-agenda-mode-map
	    "j" 'evil-next-line
	    "k" 'evil-previous-line))

;; Universal preferences
;;;; Function
(electric-pair-mode 1)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
;; When editing symlinks, allow Emacs to access Git things
(setq vc-follow-symlinks 1)

;;;; Keybindings
(general-define-key
 :prefix "C-c"
 ;; Org agenda
 "a" 'org-agenda
 "c" 'org-capture)

;;;; Appearance
(setq inhibit-splash-screen 1)
(setq inhibit-startup-message 1)
(setq initial-scratch-message "")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'after-init-hook '(lambda () (org-agenda nil "n")))
(global-font-lock-mode 1)

(setq-default display-line-numbers-type 'relative)

(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-commentary auctex exec-path-from-shell general evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
