(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
