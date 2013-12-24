(require 'package)

(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'use-package)

(global-unset-key (kbd "s-m"))

(use-package magit 
  :bind (("M-g s" . magit-status)
	 ("M-g b" . magit-blame-mode)
	 ("s-m m" . magit-status))
  :init (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
  :ensure t)

(use-package window-number
  :init (progn
	  (window-number-mode 1)
	  (window-number-meta-mode 1))
  :ensure t)

(use-package unicode-fonts
  :init (unicode-fonts-setup)
  :ensure t)

(use-package quail
  :init (progn 
	  (quail-define-package
	   "cyrillic-dvorak" "Cyrillic" "ЙЦУК" nil
	   "ЙЦУКЕН keyboard layout widely used in Russia (ISO 8859-5 encoding)
  in assuming that your default keyboard layout is dvorak"
	   nil t t t t nil nil nil nil nil t)
	  (quail-define-rules ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8) ("9" ?9) ("0" ?0) ("[" ?-) ("]" ?=) ("`" ?ё) ("'" ?й) ("," ?ц) ("." ?у) ("p" ?к)
	   ("y" ?е) ("f" ?н) ("g" ?г) ("c" ?ш) ("r" ?щ) ("l" ?з) ("/" ?х) ("=" ?ъ) ("a" ?ф)
	   ("o" ?ы) ("e" ?в) ("u" ?а) ("i" ?п) ("d" ?р) ("h" ?о) ("t" ?л) ("n" ?д) ("s" ?ж)
	   ("-" ?э) ("\\" ?\\) (";" ?я) ("q" ?ч) ("j" ?с) ("k" ?м) ("x" ?и) ("b" ?т) ("m" ?ь)
	   ("w" ?б) ("v" ?ю) ("z" ?.) ("!" ?!) ("@" ?\") ("#" ?') ("$" ?\;) ("%" ?%) ("^" ?:)
	   ("&" ??) ("*" ?*) ("(" ?() (")" ?)) ("{" ?_) ("}" ?+) ("~" ?Ё) ("\"" ?Й) ("<" ?Ц)
	   (">" ?У) ("P" ?К) ("Y" ?Е) ("F" ?Н) ("G" ?Г) ("C" ?Ш) ("R" ?Щ) ("L" ?З) ("?" ?Х)
	   ("+" ?Ъ) ("A" ?Ф) ("O" ?Ы) ("E" ?В) ("U" ?А) ("I" ?П) ("D" ?Р) ("H" ?О) ("T" ?Л)
	   ("N" ?Д) ("S" ?Ж) ("_" ?Э) ("|" ?/) (":" ?Я) ("Q" ?Ч) ("J" ?С) ("K" ?М) ("X" ?И)
	   ("B" ?Т) ("M" ?Ь) ("W" ?Б) ("V" ?Ю) ("Z" ?,))
	  (setq default-input-method "cyrillic-dvorak")))

(use-package multiple-cursors
  :bind (("C-c a" . mc/edit-lines)
	 ("C-c m" . mc/mark-all-like-this))
  :ensure t)

(use-package dired-details
  :init (setq dired-use-ls-dired nil)
  :ensure t)

(use-package dired-details+ :ensure t)

(use-package htmlize :ensure t)

(use-package org
  :init (progn 
	  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
	  (defun new-mail (topic)
	    "Start typing org-mode mail now"
	    (interactive "sTOPIC: ")
	    (if (= (length topic) 0)
		(set 'topic "no-subj"))
	    (let ((tmpfile (concat
			    "~/dropbox/notes/mails/"
			    (format-time-string "%y-%m-%d %H:%M ")
			    topic ".org")))
	      (copy-file "~/.emacs.d/templates/mail.org"
			 tmpfile)
	      (find-file tmpfile)
	      (next-line 5)))
	  (setq org-export-allow-BIND t
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
				     ("screen" . shell-script)))
	  (org-babel-do-load-languages
	   'org-babel-load-languages
	   '(   (sh . t)
		(python . t)
		(R . t)
		(ruby . t)
		(ditaa . t)
		(dot . t)
		(octave . t)
		(sqlite . t)
		(perl . t)
		(ledger . t) )))
  :config (progn
	    (require 'ob)
	    (require 'ob-eval)

	    (defvar org-babel-default-header-args:ecl
	      '((:results . "html") (:exports . "results"))
	      "Default arguments to use when evaluating a pygment source block.")

	    (defun org-babel-execute:ecl (body params)
	      "Execute a block of Dot code with org-babel.
This function is called by `org-babel-execute-src-block'."
	      (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
		     (out-file (cdr (assoc :file params)))
		     (cmdline (cdr (assoc :cmdline params)))
		     (in-file (org-babel-temp-file "ecl-"))
		     (cmd (concat org-ecl-path
				  " " cmdline
				  " " (org-babel-process-file-name in-file)
				  )))
		(unless (file-exists-p org-ecl-path)
		  (error "Could not find pygment at %s" org-ecl-path))
		(message (concat "Running Pygment: " cmd))
		(with-temp-file in-file (insert body))
		(org-babel-eval cmd "")
		))

	    (defun org-babel-prep-session:ecl (session params)
	      "Return an error because ECL does not support sessions."
	      (error "ECL does not support sessions"))

	    (setq org-ecl-path "/Users/ivaninozemtsev/dropbox/solutions/prepare.py")
	    (setq org-babel-load-languages (append org-babel-load-languages '((ecl . t)))))

  :ensure t)

(use-package load-dir
  :init (setq load-dirs t)
  :ensure t)

(use-package graphviz-dot-mode
  :init (setq graphviz-dot-view-command "dot -Tpng %s | open -f -a Preview.app")
  :ensure t)


(use-package ledger-mode :ensure t)

(use-package direx
   :init (setq direx:closed-icon "▶ "
	       direx:open-icon "▼ ")
   :bind ("C-x j" . direx:jump-to-directory)
   :ensure t)

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

(use-package cider :ensure t)
(use-package dedicated :ensure t)

(use-package find-file-in-git-repo
  :bind ("s-f" . find-file-in-git-repo)
  :ensure t)

(use-package jump-char
  :bind (("M-m" . jump-char-forward) 
	 ("M-S-m" . jump-char-backward))
  :ensure t)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Appearance
(tool-bar-mode -1)
(setq menu-bar-mode nil)
(scroll-bar-mode -1)
(setq confirm-kill-emacs 'y-or-n-p)
(setq column-number-mode t)
(server-start)

(use-package 
  edit-server
  :init (edit-server-start)
  :ensure t)

(use-package scala-mode2 :ensure t)

(use-package ensime 
  :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  :ensure t)


(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq highlight-nonselected-windows t)
