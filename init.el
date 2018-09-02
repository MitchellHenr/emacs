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

(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)

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
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-hook 'after-init-hook '(lambda () (org-agenda nil "n")))

;; Smooth scrolling
(setq scroll-margin 2)
(setq scroll-conservatively 1000)

(global-font-lock-mode 1)
(global-hl-line-mode 1)

(setq-default display-line-numbers-type 'relative)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-prettify-symbols-mode 1)

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package tex
  :ensure auctex
  :defer 1
  :config
  (setq Tex-tree-roots '("~/.texlive2017" "~/texlive2016"))
  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'before-save-hook 'hmm-tex-add-timestamp)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)

  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.sty\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.bbl\\'" . LaTeX-mode))

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


  :config
  (evil-ex-define-cmd "h[elp]" 'help)
  (evil-ex-define-cmd "W[rite]" 'evil-save)
  (evil-ex-define-cmd "E[dit]" 'evil-edit)

  (setq evil-emacs-state-modes (list 'magit-popup-mode))
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (setq evil-vsplit-window-left -1)

  (evil-mode 1))

(use-package evil-commentary
  :requires evil
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :requires evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-easymotion
  :requires evil
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-leader
  :requires evil
  :ensure t
  :config
  (evil-leader/set-key "SPC" 'evil-ex-nohighlight)
  (global-evil-leader-mode 1))

(use-package org
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (setq org-agenda-files
	'("~/Todo/school/" "~/Todo/life"))
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

(general-define-key
 "C-x g" 'magit-status
 "M-n" 'make-frame
 "C-c a" 'org-agenda
 "C-c c" 'org-capture
 :states 'normal
 "C-k" 'evil-window-up
 "C-j" 'evil-window-down
 "C-h" 'evil-window-left
 "C-l" 'evil-window-right
 "C-c h" 'help
 "<return>" 'hmm-lower-line
 :states '(normal visual)
 "<up>" 'evil-previous-visual-line
 "<down>" 'evil-next-visual-line)

(general-define-key
 :keymaps 'org-agenda-mode-map
 :states 'normal
 "j" 'evil-next-line
 "k" 'evil-previous-line
 "t" 'org-agenda-todo
 "<return>" 'org-agenda-switch-to
 "f" 'org-agenda-fortnight-view
 "r" 'org-agenda-redo)

(general-define-key
 :keymaps 'magit-mode-map
 :states 'normal
 "c" 'magit-commit
 "s" 'magit-stage
 "u" 'magit-unstage)

(general-define-key
 :keymaps 'Buffer-menu-mode-map
 :states 'normal
 "k" 'evil-previous-line
 "<return>" 'Buffer-menu-this-window)

(general-define-key
 :keymaps 'help-mode-map
 :states 'normal
 "q" 'quit-window)

(defun hmm-lower-line ()
  "Add a line above, without moving point"
  (interactive "*")
  (let ((col (current-column)))
    (evil-open-above 1)
    (evil-next-line)
    (move-to-column col)
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
