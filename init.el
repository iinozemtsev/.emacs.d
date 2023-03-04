(require 'package)

(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
(require 'use-package)

(use-package magit 
  :bind (("M-g s" . magit-status)
	 ("M-g b" . magit-blame-mode)
	 ("s-m m" . magit-status))
  :init (setq
         magit-emacsclient-executable "/usr/bin/emacsclient")
  :ensure t)


(setq visible-bell t)
(setq ring-bell-function 'ignore)

(use-package window-numbering
  :init 
  (window-numbering-mode 1)
  :ensure t)

(use-package multiple-cursors
  :bind (("C-c a" . mc/edit-lines)
	 ("C-c m" . mc/mark-all-like-this))
  :ensure t)

(use-package htmlize :ensure t)

(use-package org
  :init (progn 
	  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
	  (setq org-tags-column 0
		org-export-allow-BIND t
		org-export-allow-bind-keywords t
		org-confirm-babel-evaluate nil
		org-src-lang-modes '(("ecl" . fundamental)
				     ("ocaml" . tuareg)
				     ("elisp" . emacs-lisp)
				     ("ditaa" . artist)
				     ("asymptote" . asy)
				     ("dot" . graphviz-dot)
				     ("sqlite" . sql)
				     ("calc" . fundamental)
				     ("C" . c)
				     ("cpp" . c++)
				     ("screen" . shell-script))
                org-log-done 'time
		org-agenda-files (list "~/notes")
		org-default-notes-file "~/notes/inbox.org"
		org-refile-targets  '((nil :maxlevel . 9)
				    (org-agenda-files :maxlevel . 9)))
	  (defun sacha/org-html-checkbox (checkbox)
	    "Format CHECKBOX into HTML."
	    (case checkbox (on "<span class=\"check\">&#x2713;</span>") ; checkbox (checked)
		  (off "<span class=\"checkbox\">&#x2717;</span>")
		  (trans "<code>[-]</code>")
		  (t "")))
	  (defadvice org-html-checkbox (around sacha activate)
	    (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0)))))
  :bind (("s-t" . org-todo-list)
	 ("s-s" . org-schedule)
	 ("s-a" . org-agenda)
	 ("s-c" . org-capture))
  :ensure t)

(use-package load-dir
  :init (setq load-dirs t)
  :ensure t)


(use-package s :ensure t)

(use-package smartparens
  :config (progn
	    (require 'smartparens-config)
	    (setq sp-base-key-bindings 'paredit
		  sp-autoskip-closing-pair 'always)
	    (sp-use-paredit-bindings)
	    (show-smartparens-global-mode +1)
	    (smartparens-global-mode +1)
	    (global-set-key (kbd "M-(") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
	    (global-set-key (kbd "M-[") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
	    (global-set-key (kbd "M-\"") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\""))))
  :ensure t)

(use-package dedicated :ensure t)

(setq blink-matching-paren nil)
;; Appearance
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(if (display-graphic-p)
    (scroll-bar-mode -1))
(setq confirm-kill-emacs 'y-or-n-p)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(if (display-graphic-p)
    (server-start))


(if (display-graphic-p)
    (use-package 
      edit-server
      :init (edit-server-start)
      :ensure t))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


(defvar pre-ediff-window-configuration nil
  "window configuration to use")
(defvar new-ediff-frame-to-use nil
  "new frame for ediff to use")
(defun save-my-window-configuration ()
  (interactive)
  (setq pre-ediff-window-configuration (current-window-configuration))
  (select-frame-set-input-focus (setq
				 new-ediff-frame-to-use (new-frame))))

(add-hook 'ediff-before-setup-hook 'save-my-window-configuration)
(defun restore-my-window-configuration ()
  (interactive)
  (when (framep new-ediff-frame-to-use)
    (delete-frame new-ediff-frame-to-use)
    (setq new-ediff-frame-to-use nil))
  (when (window-configuration-p pre-ediff-window-configuration
				(set-window-configuration pre-ediff-window-configuration))))
(add-hook 'ediff-after-quit-hook-internal 'restore-my-window-configuration)


(setq highlight-nonselected-windows t)
(add-hook 'prog-mode-hook 'subword-mode)

(use-package wgrep
  :ensure t)
;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

(global-set-key (kbd "C-|")
		(lambda (&optional arg) (interactive "P")
		  (let ((windows (window-list)))
		    (when (= 2 (length windows))
		      (let ((first-window (car windows))
			    (second-buffer (window-buffer (nth 1 windows))))
			(delete-other-windows first-window)
			(set-window-buffer (split-window-right) second-buffer))))))

(defun kill-back () "" (interactive) (kill-sexp -1))
(global-set-key [C-M-backspace] 'kill-back)

(use-package markdown-mode :ensure t)

(use-package lua-mode :ensure t)

(setq large-file-warning-threshold 150000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org window-numbering wgrep use-package swiper smartparens s php-mode nginx-mode multiple-cursors markdown-mode magit lua-mode load-dir ledger-mode image+ hydra htmlize haskell-mode find-file-in-project easy-kill direx dedicated company cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
