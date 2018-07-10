;;; package --- Init file
;;; Commentary:
;;; This be me Emacs init file
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(exec-path-from-shell-initialize)
(eval-when-compile
  (require 'use-package))
(defconst user-init-directory
  "~/.emacs.d/config")

(defun load-user-file (file)
  "Load a file FILE in current user's configuration directory."
  (interactive "File to load: ")
  (load-file (expand-file-name file user-init-directory)))

(load-user-file "appearance-config.el")
(load-user-file "evil-config.el")
(load-user-file "editing-config.el")
(load-user-file "python-config.el")
(load-user-file "headers-config.el")

;; ABBREVS
(setq-default abbrev-mode t)
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")

;; MODES
(global-flycheck-mode 1)
(electric-pair-mode 1)
(ivy-mode 1)
(which-key-mode 1)
(show-paren-mode 1)
(hl-line-mode 1)

;; AUTOCOMPLETION

;; GENERAL HOOKS
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; AUCTEX
(setq-default TeX-master nil) ; Tries to find a master file.
(setq TeX-parse-self t) ; Enable parse on load.
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

;; ORG
(require 'org-agenda)
(define-key org-agenda-mode-map "t" 'butterfly)
(define-key org-agenda-mode-map "j" 'evil-next-line)
(define-key org-agenda-mode-map "k" 'evil-previous-line)
(setq org-agenda-files (append (file-expand-wildcards "~/UVM/*/*/*.org") '("~/Todo/life/list.org")))
(setq org-default-notes-file "~/Todo/notes.org")
(define-key global-map "\C-cc" 'org-capture)

;; RUBY
(add-hook 'ruby-mode-hook
	  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
		`(ruby-mode
		  ,(rx (or "def" "class" "if" "while" "else" "module" "do" "{" "[")) ; Block start
		  ,(rx (or "}" "]" "else" "end"))                       ; Block end
		  ,(rx (or "#" "=begin"))                        ; Comment start
		  ruby-forward-sexp nil)))

;; BACKUPS
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-list
   (quote
    ((44 "qc" "" nil)
     (54 "partial" "" nil)
     (61 "implies" "" nil)
     (56 "infty" "" nil)
     (101 "varepsilon" "" nil))))
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(compilation-message-face (quote default))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("12b204c8fcce23885ce58e1031a137c5a14461c6c7e1db81998222f8908006af" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" default)))
 '(fci-rule-color "#073642")
 '(global-prettify-symbols-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(linum-relative-current-symbol "->")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/UVM/EE/183/EE183.org" "~/UVM/EE/120/EE120.org" "~/UVM/HON/201/HON201.org" "~/UVM/HON/101/HON101.org" "~/UVM/MATH/330/MATH330.org" "~/UVM/MATH/300/MATH300.org" "~/UVM/MATH/273/MATH273.org" "~/UVM/MATH/266/MATH266.org" "~/UVM/MATH/235/MATH235.org" "~/UVM/MATH/230/MATH230.org" "~/UVM/MATH/052/MATH052.org" "~/UVM/PHYS/273/PHYS273.org" "~/UVM/PHYS/214/PHYS214.org" "~/UVM/PHYS/213/PHYS213.org" "~/UVM/PHYS/211/PHYS211.org" "~/UVM/thesis/proposal/prop.org" "~/UVM/thesis/proposal/thesis.org" "~/Todo/life/list.org")))
 '(org-agenda-start-on-weekday 0)
 '(package-selected-packages
   (quote
    (cdlatex dockerfile-mode solarized-theme linum-relative py-autopep8 elpy auctex hl-spotlight header2 ample-zen-theme markdown-mode+ markdown-mode vi-tilde-fringe auto-complete go-mode key-chord neotree paganini-theme auto-yasnippet which-key syndicate popup org-evil monokai-theme julia-mode ivy flycheck exec-path-from-shell evil-visual-mark-mode evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-leader evil-goggles evil-easymotion evil-commentary)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(python-shell-interpreter "python3")
 '(require-final-newline nil)
 '(ring-bell-function (quote ignore))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here
