(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(if (display-graphic-p)
    (scroll-bar-mode -1))
(setq confirm-kill-emacs 'y-or-n-p)
(if (display-graphic-p)
    (server-start))

(use-package window-numbering
  :init 
  (window-numbering-mode 1)
  :ensure t)

(use-package magit 
  :bind (("M-g s" . magit-status))
  :ensure t)
