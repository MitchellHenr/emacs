(package-initialize)

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq load-path (cons "~/emacs/lisp"
		      load-path))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar backup-dir "~/.emacs.d/backups")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(server-start)


;; Universal preferences

;;;; Function
(electric-pair-mode 1)
(show-paren-mode 1)

(setq shell-multiple-shells t)

(setq confirm-kill-emacs 'yes-or-no-p)
(setq auto-save-default nil)
;; When editing symlinks, allow Emacs to access Git things
(setq vc-follow-symlinks 0)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ns-pop-up-frames t)
(setq message-log-max t)

;; Restoring sessions
(setq desktop-save 'if-exists)
(desktop-save-mode 1)
(setq desktop-restore-eager 1)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
		(file-name-history        . 100)
		(grep-history             . 30)
		(compile-history          . 30)
		(minibuffer-history       . 50)
		(query-replace-history    . 60)
		(read-expression-history  . 60)
		(regexp-history           . 60)
		(regexp-search-ring       . 20)
		(search-ring              . 20)
		(shell-command-history    . 50)
		tags-file-name
		register-alist)))


;;;; Appearance

;; Startup
(setq inhibit-splash-screen 1)
(setq inhibit-startup-message 1)
(setq initial-scratch-message "")
(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-hook 'after-init-hook '(lambda () (org-agenda nil "n")))

;; Smooth scrolling
(setq scroll-margin 2)
(setq scroll-step 1)
(setq hscroll-step 1)
(setq ring-bell-function 'ignore)

(global-font-lock-mode 1)
(global-hl-line-mode 1)

(setq-default display-line-numbers-type 'visual)
(setq-default display-line-numbers-current-absolute t)
(setq-default display-line-numbers-widen t)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-prettify-symbols-mode 1)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "if" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;; Packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-tree-roots '("~/.texlive2017" "~/.texlive2016" "~/.texlive2018"))
  (setq-default TeX-master "../main")
  (setq TeX-parse-self t)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'abbrev-mode)

  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.sty\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.bbl\\'" . LaTeX-mode))

  (setq LaTeX-math-list
	'((?, "qc" "" nil)
	  (?6 "partial" "" nil)
	  (?= "implies" "" nil)
	  (?8 "infty" "" nil)
	  (?T "dagger" "" nil)
	  (?e "varepsilon" "" nil))))

(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package jedi
  :ensure t)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package org
  :config
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (setq org-agenda-files
	'("~/Todo/school/" "~/Todo/life" "~/apps/"))
  (setq org-agenda-start-day "0d")
  (setq org-agenda-span 7)
  (setq org-agenda-start-on-weekday nil))

(use-package which-key
  :defer 1
  :config
  (which-key-mode 1))

(use-package highlight-symbol
  :ensure t
  :defer 1
  :config
  (setq-default highlight-symbol-idle-delay 1)
  (highlight-symbol-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package browse-kill-ring
  :ensure t)

(use-package sed-mode
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-hard))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package avy
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t
  :requires visual-regexp)

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode)
  (setq dimmer-fraction 0.4))

(general-define-key ;; General
 :keymaps 'global
 "C-x g" 'magit-status
 "M-n" 'make-frame
 "M-x" 'counsel-M-x
 "C-s" 'swiper)

(general-define-key
 :prefix "C-c"
 "a" 'org-agenda
 "c" 'org-capture
 "r" 'vr/replace
 "q" 'vr/query-replace
 "C-s" 'vr/isearch-forward
 "C-r" 'vr/isearch-backward)

(general-define-key
 :prefix "C-\\"
 "w" 'avy-goto-word-0
 "W" 'avy-goto-word-1
 "f" 'avy-goto-char
 "F" 'avy-goto-char-2
 "j" 'avy-goto-line)

(general-define-key ;; Org
 :keymaps 'org-agenda-mode-map
  "j" 'evil-next-line
  "<return>" 'org-agenda-switch-to
  "f" 'org-agenda-fortnight-view
  "r" 'org-agenda-redo)

(defun hmm-lower-line ()
  "Add a line above, without moving point"
  (interactive "*")
  (let ((col (current-column)))
    (evil-open-above 1)
    (evil-next-line)
    (move-to-column col)
    (evil-forward-char)
    (evil-force-normal-state))
  nil)

(defun hmm-tex-add-timestamp ()
  "Add a timestamp to the last line of a tex file"
  (interactive "*")
  (when (eq major-mode 'latex-mode)
    (save-excursion
      (save-window-excursion
	(save-restriction
	  (end-of-buffer)
	  (move-beginning-of-line nil)
	  (if (not (search-forward "%% Last updated: " nil t 1))
	      (progn
		(move-end-of-line nil)
		(insert "\n%% Last updated: "))
	    (kill-line))
	  (insert (current-time-string))))))
  nil)
(put 'scroll-left 'disabled nil)
