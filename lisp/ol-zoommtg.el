;;; ol-zoommtg.el - Support for zoom links in Org mode
(require 'ol)

(org-link-set-parameters "zoommtg"
			 :follow #'org-zoommtg-command
			 :export #'org-zoommtg-export)

(defun org-zoommtg-command (link)
    (call-process-shell-command (format "open \"zoommtg:%s\"" link) nil nil))

(defun org-zoommtg-export (link description format)
  "Export a Zoom meeting link from Org files."
  (let ((desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"zoommtg:%s\">%s</a>" link desc))
      (`latex (format "\\href{zoommtg:%s}{%s}" link desc))
      (`texinfo (format "@uref{zoommtg:%s,%s}" link des))
      (`ascii (format "%s (zoommtg:%s)" desc link))
      (_ link))))

(provide 'ol-zoommtg)
;;; ol-zoommtg.el ends here
