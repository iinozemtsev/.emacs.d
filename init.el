(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

'(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode 0)

(use-package general :straight t)

(use-package bazel :straight t)

(use-package gn-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode))
  (add-to-list 'auto-mode-alist '("\\.gni\\'" . gn-mode)))

(use-package consult :straight t)

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :general
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert    ; Choose selected candidate
   "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
   ;; NOTE 2022-02-05: Cycle through candidate groups
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group)
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize 'grow-only)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(use-package swift-mode :straight t)

(use-package dired
  :init
  (defun my-hide-details ()
    (dired-hide-details-mode 1))
  :hook ('dired-mode . my-hide-details))

(use-package smartparens :straight t
  :config
  (require 'smartparens-config)
  :init
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  (sp-use-paredit-bindings)
  :custom (sp-escape-quotes-after-insert nil))

(if (display-graphic-p)
    (scroll-bar-mode -1))

(setq confirm-kill-emacs 'y-or-n-p)
(if (display-graphic-p)
    (server-start))

(use-package window-numbering
  :init 
  (window-numbering-mode 1)
  :straight t)

(use-package pcre2el
  :config (pcre-mode)
  :straight t)

(use-package magit 
  :bind (("M-g s" . magit-status))
  :straight t)

(use-package markdownfmt
  :straight t)

(set-frame-font "JetBrains Mono 12" nil t)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t) :straight t)

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-right-command-modifier 'super)
      (set-frame-font "JetBrains Mono 20" nil t)))

(use-package dart-mode :straight t)
(use-package markdown-mode :straight t)
(use-package vscdark-theme :straight t)

(use-package multiple-cursors
  :bind (("C-c a" . mc/edit-lines)
         ("C-c m" . mc/mark-all-like-this))
  :straight t)

(defun toggle-quotes ()
  "Toggle single/double quotes and flip the internal quotes."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c
            (cl-case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))
(global-set-key (kbd "M-\"") 'toggle-quotes)
(global-set-key (kbd "M-g f") 'project-find-file)
(global-set-key (kbd "C-c C-r") 'ff-find-other-file)
(setq-default indent-tabs-mode nil)

(use-package company :straight t
  :config (global-company-mode))

(use-package eglot
  :bind
  ("C-c C-f" . eglot-format-buffer)
  ("M-g ?" . eldoc-doc-buffer)
  ("M-g u" . eglot-code-actions)
  ("M-RET" . eglot-code-actions)
  ("M-g e" . eglot-rename)
  ("M-n n ". flymake-goto-next-error)
  ("M-n p ". flymake-goto-prev-error)
  ("M-g i ". eglot-inlay-hints-mode)
  ("M-/" . company-complete))
