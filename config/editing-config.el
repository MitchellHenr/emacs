;;; package --- summary:
;;; Preferences for my Emacs config that have to do with editing

;;; Commentary:
;;; Includes key bindings, etc

;;; Code:

;;; Keybindings:
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-l" 'evil-window-right)
(global-set-key "\C-h" 'evil-window-left)
(global-set-key "\C-k" 'evil-window-up)
(global-set-key "\C-j" 'evil-window-down)
(global-set-key "\C-ch" 'help)

(use-package auto-complete
  :config
  (global-auto-complete-mode 1))

;;; Headers:
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)

(autoload 'auto-make-header "header2")


(provide 'editing-config)
;;; editing-config.el ends here
