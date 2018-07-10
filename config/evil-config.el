;;; package --- summary:

;;; Commentary:

;; EVIL
;;; Code:
(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol 1)
  :config
  (evil-mode 1)
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (setq evil-vsplit-window-left 1)

  (evil-ex-define-cmd "W" 'evil-write)
  (evilem-default-keybindings "SPC")
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-commentary-mode 1)
  (evil-search-highlight-persist 1)
  (evil-visual-mark-mode 1))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.050)
  )

(provide 'evil-config)
;;; evil-config.el ends here
