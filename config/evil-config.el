;;; package --- summary:

;;; Commentary:

;; EVIL
;;; Code:
(use-package evil
  :init
  :config
  (evil-mode 1)
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)

  (setq evil-vsplit-window-right 1)

  (evil-ex-define-cmd "W" 'evil-write)
  (evilem-default-keybindings "SPC")
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-commentary-mode 1)
  (evil-goggles-mode 1)
  (evil-search-highlight-persist 1)
  (evil-visual-mark-mode 1))

;; (when (require 'evil-collection nil t)
;;   (evil-collection-init))

(provide 'evil-config)
;;; evil-config.el ends here
