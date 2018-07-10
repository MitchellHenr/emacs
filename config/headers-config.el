;;; package --- summary:
;;; Preferences for my Emacs config that have to do with headers

;;; Commentary:
;;; Includes key bindings, etc

;;; Code:

;;; Headers:
(require 'header2)

(defsubst my/header-filename ()
  "Insert filename in the header."
  (insert header-prefix-string "Filename:      " (file-name-nondirectory (buffer-file-name)) "\n"))

(defsubst my/header-timestamp ()
  "Insert field for timestamp."
  (insert header-prefix-string "Creation date: " (current-time-string) "\n"))

(defsubst my/header-author ()
  "Insert author name."
  (insert header-prefix-string "Author:        Henry Mitchell\n"))

(defsubst my/header-description ()
  "Insert \"Description: \" line."
  (insert header-prefix-string "Description:   \n"))

(defsubst my/header-update-count ()
  "Insert a count of the number of times the file has been saved."
  (insert header-prefix-string "Update count:  0\n"))

(defsubst my/header-update-time ()
  "Insert the last time the file was saved."
  (insert header-prefix-string "Last updated:  " (current-time-string) "\n"))

(defsubst my/header-python-imports ()
  "Insert imports for Python script."
  (insert "import matplotlib.pyplot as plt\nimport numpy as np\nimport pandas as pd\n"))

;;; Updates
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)

(defun my/update-last-modified-date ()
  "Update the last-modeified line."
  (delete-and-forget-line)
  (insert (current-time-string)))

(register-file-header-action "Update count:  " 'update-write-count)
(register-file-header-action "Last updated:  " 'my/update-last-modified-date)

;;; New files
(autoload 'auto-make-header "header2")

(setq make-header-hook '(my/header-filename
			 my/header-author
			 my/header-timestamp
			 my/header-update-time
			 my/header-update-count
			 my/header-description
			 (if (equal major-mode 'python-mode)
			     my/header-python-imports)))


(add-hook 'python-mode-hook 'auto-make-header)


(provide 'headers-config)
;;; headers-config.el ends here
