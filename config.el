(server-start)
(defun raise-emacs-on-aqua()
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(defvar backup-dir "~/.emacs.d/backups")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq ispell-program-name "/usr/local/bin/aspell")

;;;; Function
(electric-pair-mode 1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(setq shell-multiple-shells t)

(setq confirm-kill-emacs 'yes-or-no-p)
(setq auto-save-default nil)
;; When editing symlinks, allow Emacs to access Git things
(setq vc-follow-symlinks t)
;; *scratch* buffers start in org mode
(setq initial-major-mode 'org-mode)
(setq default-major-mode 'org-mode)

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(setq echo-keystrokes 0.02)

(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq-default dired-listing-switches "-alGFh")

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")
(setq ns-pop-up-frames nil)
(setq message-log-max t)
(setq ok-if-already-exists t)


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

(when (>= emacs-major-version 27)
  (setq what-cursor-show-names 1)
  (setq auto-save-no-message 1)
  (global-tab-line-mode 1))

(global-font-lock-mode 1)
(global-hl-line-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)
(fringe-mode '(0 . 0))

;; Startup
(setq inhibit-splash-screen 1)
(setq inhibit-startup-message 1)
(setq initial-scratch-message "")
(setq truncate-partial-width-windows 20)
(setq-default truncate-lines nil)
(add-to-list 'default-frame-alist '(font . "Andale Mono-12"))
(set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-hook 'after-init-hook '(lambda () (org-agenda nil "n")))

;; Smooth scrolling
(setq scroll-margin 2)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)
(setq hscroll-step 1)
(setq ring-bell-function 'ignore)
(setq redisplay-dont-pause 1)

(setq-default display-line-numbers-type 'visual)
(setq-default display-line-numbers-current-absolute t)
(setq-default display-line-numbers-widen t)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
		`(ruby-mode
		  ,(rx (or "def" "class" "if" "module" "do" "{" "[")) ; Block start
		  ,(rx (or "}" "]" "end"))                       ; Block end
		  ,(rx (or "#" "=begin"))                        ; Comment start
		  ruby-forward-sexp nil)))

(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package org
  :ensure org-plus-contrib
  ;; :pin org
  :ensure auctex
  :ensure htmlize
  :config
  (require 'ox-extra)
  (require 'ol-zoommtg)
  (require 'org-tempo)
  (require 'latex)
  (ox-extras-activate '(ignore-headlines))
  ;; (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'highlight-symbol-mode)
  (add-hook 'org-mode-hook 'LaTeX-math-mode)
  (add-hook 'org-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook (lambda ()
			     (setq-local electric-pair-pairs (append
							      electric-pair-pairs
							      '((?\$ . ?\$))))))
  (add-hook 'org-mode-hook (lambda ()
			     (modify-syntax-entry ?< "_")))
  (add-hook 'org-mode-hook (lambda ()
			     (modify-syntax-entry ?> "_")))
  (add-hook 'org-mode-hook (lambda ()
			     (setq ispell-extra-args '("-t"))))
  (setq org-agenda-files
	'("~/Todo/school/" "~/Todo/life" "~/Todo/work" "~/apps/apps.org"))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-span 7)
  (setq org-agenda-start-day "0d")
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-hide-tags-regexp "ignore\\|noexport")
  (setq org-blank-before-new-entry
	'((heading . t) (plain-list-item . auto)))
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (setq org-deadline-warning-days 4)
  (setq org-export-in-background t)
  (setq org-link-file-path-type 'adaptive)
  (setq org-log-done 'time)
  (setq org-pretty-entities 1))
(setq org-src-tab-acts-natively t)
(org-reload)

(use-package diminish
  :ensure t
  :config
  (diminish 'flyspell-mode)
  (diminish 'highlight-symbol-mode)
  (diminish 'visual-line-mode)
  (diminish 'reftex-mode)
  (diminish 'iimage-mode)
  (diminish 'abbrev-mode))

(use-package latex
  :defer 1
  :ensure auctex
  :config
  (setq TeX-tree-roots '("~/.texlive2017" "~/texlive2016" "~/.texlive2018"))
  (setq-default TeX-master "../main")
  (setq-default TeX-PDF-mode t)
  (setq TeX-parse-self t)
  (setq TeX-auto-untabify t)
  (setq TeX-insert-braces nil)
  (setq TeX-source-correlate-mode t)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'abbrev-mode)
  (add-hook 'bibtex-mode-hook 'hs-minor-mode)
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (add-to-list 'TeX-output-view-style
			   '("^pdf$" "."
			     "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
	    )

  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.sty\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.bbl\\'" . LaTeX-mode))

  (add-to-list 'safe-local-variable-values '(TeX-command-extra-options . (regexp-quote "-jobname='[^']*'")))

  (setq LaTeX-electric-left-right-brace nil)

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

(use-package evil
  :ensure t

  :init
  (setq evil-want-integration nil)
  (setq evil-search-module 'evil-search)
  (setq evil-want-Y-yank-to-eol 1)


  :config
  (evil-ex-define-cmd "h[elp]" 'help)
  (evil-ex-define-cmd "W[rite]" 'evil-save)
  (evil-ex-define-cmd "e[dit]" 'ido-find-file)
  (evil-ex-define-cmd "E[dit]" 'ido-find-file)
  (evil-ex-define-cmd "b[uffer]" 'ido-switch-buffer)
  (evil-ex-define-cmd "B[uffer]" 'ido-switch-buffer)

  (setq evil-emacs-state-modes (list 'magit-popup-mode))
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (setq evil-vsplit-window-left -1)

  (evil-mode 1))

(use-package evil-leader
  :requires evil
  :ensure t
  :config
  (evil-leader/set-key "SPC" 'evil-ex-nohighlight)
  (global-evil-leader-mode 1))

(use-package evil-commentary
  :requires evil
  :ensure t
  :diminish
  :config
  (evil-commentary-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (setq comment-start "%% "))))

(use-package evil-surround
  :requires evil
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (push '(?$ . ("$" . "$")) evil-surround-pairs-alist))))

(use-package evil-easymotion
  :requires evil
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(use-package evil-goggles
  :requires evil
  :ensure t
  :diminish
  :config
  (setq evil-goggles-duration 0.100)
  (evil-goggles-mode))

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

(use-package swiper
  :diminish
  :ensure t)

(use-package counsel
  :diminish
  :ensure t
  :config
  (counsel-mode 1))

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package jedi
  :ensure t)

(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1))

(use-package highlight-symbol
  :ensure t
  :defer 1
  :config
  (setq-default highlight-symbol-idle-delay 0.5)
  (highlight-symbol-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package browse-kill-ring
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (color-theme-sanityinc-tomorrow-eighties))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode)
  (setq dimmer-fraction 0.4))

(use-package xml+
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package magic-latex-buffer
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
  (when (require 'diminish nil 'noerror)
    (diminish 'magic-latex-buffer)))

(use-package julia-mode
  :defer 1
  :ensure t)

(use-package magit
  :ensure t
  :general
  ("C-x g" 'magit-status))

(use-package evil-magit
  :ensure t)

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-enable-flex-matching t))

(use-package csv-mode
  :ensure t)

(use-package htmlize
  :ensure t)

(general-define-key ;; General
 :states '(normal visual)
 "<down>" 'evil-next-visual-line
 "RET" 'hmm-lower-line
 "<up>" 'evil-previous-visual-line
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right
 "C-t" 'transpose-chars
 "M-n" 'make-frame
 "M-x" 'counsel-M-x
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "/" 'swiper)

(general-define-key ;; Getting to org agenda
 :prefix "C-c"
 "a" 'org-agenda
 "c" 'org-capture
 "h" 'help)

(general-define-key ;; Org agenda
 :keymaps 'org-agenda-mode-map
 :states 'normal
 "j" 'evil-next-line
 "k" 'evil-previous-line
 "t" 'org-agenda-todo
 "q" 'org-agenda-quit
 "<return>" 'org-agenda-switch-to
 "f" 'org-agenda-fortnight-view
 "r" 'org-agenda-redo
 "<right>" 'org-agenda-later
 "<down>" 'org-agenda-later
 "<up>" 'org-agenda-earlier
 "<left>" 'org-agenda-earlier)

(general-define-key ;; Org
 :keymaps 'org-mode-map
 :states 'insert
 "<return>" 'newline-and-indent)

(general-define-key ;; Org
 :keymaps 'org-mode-map
 :states 'normal
 "[" 'org-previous-visible-heading
 "]" 'org-next-visible-heading)

(general-define-key ;; Buffer menu
 :keymaps 'Buffer-menu-mode-map
 :states 'normal
 "k" 'evil-previous-line
 "<return>" 'Buffer-menu-this-window)

(general-define-key ;; Help
 :keymaps 'help-mode-map
 :states 'normal
 "<return>" 'push-button
 "q" 'quit-window)

(general-define-key ;; Dired
 :keymaps 'dired-mode-map
 :states 'normal
 "<return>" 'dired-find-file)

(general-define-key ;; Package menu
 :keymaps 'package-menu-mode-map
 :states 'normal
 "<BS>" 'package-menue-backup-unmark
 "<return>" 'package-menu-describe-package
 "U" 'package-menu-mark-upgrades
 "i" 'package-menu-mark-install
 "u" 'package-menu-mark-upgrades
 "d" 'package-menu-mark-delete
 "x" 'package-menu-execute)

(general-define-key ;; LaTeX mode
 :keymaps 'LaTeX-mode-map
 :states 'normal
 "*" 'LaTeX-environment)

(general-define-key ;; reftex TOC mode
 :keymaps 'reftex-toc-mode-map
 :states 'normal
 "SPC" 'reftex-toc-view-line
 "TAB" 'reftex-toc-goto-line
 "RET" 'reftex-toc-goto-line-and-hide
 "<" 'reftex-toc-promote
 ">" 'reftex-toc-demote
 "C-c >" 'reftex-toc-display-index
 "q" 'reftex-toc-quit
 "l" 'reftex-toc-toggle-labels
 "i" 'reftex-toc-toggle-index
 "c" 'reftex-toc-toggle-context
 "F" 'reftex-toc-toggle-file-borders
 "t" 'reftex-toc-max-level
 "f" 'reftex-toc-toggle-follow
 "g" 'revert-buffer
 "a" 'reftex-toggle-auto-toc-recenter
 "d" 'reftex-toggle-dedicated-frame
 "r" 'reftex-toc-rescan
 "." 'reftex-toc-show-calling-point
 "j" 'reftex-toc-next
 "k" 'reftex-toc-previous)

(general-unbind 'normal 'grep-mode-map
  :with 'ignore
  "<return>"
  )

(when (>= emacs-major-version 27)
  (general-define-key ;; Changing tabs
   :keymaps 'override
   "C-<left>" 'previous-buffer
   "C-<right>" 'next-buffer))

(defun hmm-lower-line ()
  "Add a line above, without moving point"
  (interactive "*")
  (let ((col (current-column)))
    (beginning-of-line)
    (newline)
    (move-to-column col)))

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

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc ’kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value ’major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))
