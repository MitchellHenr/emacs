;;; package --- summary:
;;; Preferences for my Emacs config that have to do with Python programming

;;; Commentary:
;;; Header, completion, etc

;;; Code:
(require 'py-autopep8)

(elpy-enable)

(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide 'python-config)
;;; python-config.el ends here
