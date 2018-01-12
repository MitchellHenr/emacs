;;; package --- summary:
;;; Preferences for my Emacs config that have to do with appearance

;;; Commentary:
;;; Includes theme information, font information, etc.

;;; Code:

;;; Startup facts and general window appearance
(setq inhibit-startup-screen 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(scroll-bar-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indicate-empty-lines t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Theme
(load-theme 'ample-zen 1)

;;; Face(s)
(set-face-attribute 'default nil :font "Andale Mono-12")
(set-frame-font "Andale Mono-12" nil t)

;;; Scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 101)

(use-package nlinum-relative
  :init
  (setq nlinum-relative-current-symbol "->")
  (setq nlinum-relative-redisplay-delay 0)
  :config
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

(use-package vi-tilde-fringe
  :config
  (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode))

;;; Mode bar

(provide 'appearance-config)
;;; appearance-config.el ends here
