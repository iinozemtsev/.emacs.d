;; --------------------------------------------------
;; Basic settings
;; --------------------------------------------------
(setq gc-cons-threshold (* 100 1024 1024)) ;; Speed up startup
(setq read-process-output-max (* 1024 1024))
(setq custom-file (concat user-emacs-directory "custom.el"))

;; --------------------------------------------------
;; Host-specific variables
;; --------------------------------------------------
(defvar my/emacs-profile
  (let ((profile-file (expand-file-name ".emacs-profile" user-emacs-directory)))
    (if (file-exists-p profile-file)
        ;; Read explicit .emacs-profile
        (with-temp-buffer
          (insert-file-contents profile-file)
          (string-trim (buffer-string)))
      ;; Fallback: Sanitized system-name
      (car (split-string (system-name) "\\."))))
  "The configuration profile identifier for this machine.")

;; Load the profile (defines vars like my/is-corp-network, my/font-size)
(let ((host-config (expand-file-name (format "hosts/%s.el" my/emacs-profile) user-emacs-directory)))
  (if (file-exists-p host-config)
      (load host-config)
    (message "Warning: Host config not found: %s" host-config)))

;; --------------------------------------------------
;; Bootstrap straight.el
;; --------------------------------------------------

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
(setq straight-use-package-by-default t)
(require 'use-package)

;; --------------------------------------------------
;; Real config starts here
;; --------------------------------------------------
(use-package emacs
  :custom
  (ring-bell-function 'ignore)
  (confirm-kill-emacs 'y-or-n-p)
  (column-number-mode t)
  :init
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default indent-tabs-mode nil))

(use-package general :straight t)

(use-package yaml-mode :straight t)

(use-package gn-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode))
  (add-to-list 'auto-mode-alist '("\\.gni\\'" . gn-mode)))

(use-package consult :straight t)

(use-package csv-mode :straight t)

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
  :demand t
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
  :straight nil
  :init
  :hook ('dired-mode . dired-hide-details-mode))

(use-package smartparens :straight t
  :config
  (require 'smartparens-config)
  :init
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  (sp-use-paredit-bindings)
  :custom (sp-escape-quotes-after-insert nil))

(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-12"))
  (scroll-bar-mode -1)) ;; TODO: do I need it here?

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

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

(use-package emacs
  :if (eq system-type 'darwin)
  :custom
  ;; Key Modifiers
  (mac-command-modifier 'meta)       ;; Command -> Meta
  (mac-right-command-modifier 'super) ;; Right Command -> Super

  :config
  ;; Font: Using default-frame-alist is better for Daemon/Client setups
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-20")))

(use-package dart-mode :straight t)
(use-package markdown-mode :straight t)

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (custom-safe-themes t) ;; Trust all themes so it doesn't ask "Are you sure?"
  
  :config
  ;; 1. The Toggle Function (Cleaned up)
  (defun my/toggle-theme ()
    "Toggle between Doom Bluloco Light and Dark."
    (interactive)
    (if (custom-theme-enabled-p 'doom-bluloco-light)
        (progn
          (disable-theme 'doom-bluloco-light)
          (load-theme 'doom-bluloco-dark t)
          (message "Theme: Dark"))
      ;; Else: Switch to Light
      ;; We disable the dark theme first to prevent "theme bleeding"
      ;; (where colors mix weirdly)
      (disable-theme 'doom-bluloco-dark)
      (load-theme 'doom-bluloco-light t)
      (message "Theme: Light")))

  ;; 2. Optional: Enable Doom's tweaks for Org-mode headings/lists
  ;; (It makes them look much nicer)
  (doom-themes-org-config)

  ;; 3. LOAD THE DEFAULT (The Fix!)
  ;; This ensures you start in Dark mode automatically.
  (load-theme 'doom-bluloco-light t)

  :bind
  ("<f12>" . my/toggle-theme))

(use-package multiple-cursors
  :bind (("C-c a" . mc/edit-lines)
         ("C-c m" . mc/mark-all-like-this))
  :straight t)

(use-package emacs
  :config
  (defun my/toggle-quotes ()
    "Toggle single/double quotes and flip the internal quotes."
    (interactive)
    (save-excursion
      (re-search-backward "[\"']")
      (let* ((start (point))
             (old-c (char-after start))
             new-c)
        (setq new-c
              (cond ((eq old-c ?\") "'")
                    ((eq old-c ?\') "\"")))
        (setq old-c (char-to-string old-c))
        (delete-char 1)
        (insert new-c)
        (re-search-forward old-c)
        (backward-char 1)
        (let ((end (point)))
          (delete-char 1)
          (insert new-c)
          (replace-string new-c old-c nil (1+ start) end)))))

  :bind
  ("M-\"" . my/toggle-quotes))

(use-package project
  :straight nil
  :bind
  ("M-g f" . project-find-file))

(use-package find-file
  :straight nil
  :bind ("C-c C-r" . ff-find-other-file))

(use-package company :straight t
  :config (global-company-mode))

(use-package eglot
  :straight nil
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

(use-package vc :straight nil
  :bind  ("M-g r" . vc-git-grep))

(use-package emacs
  :config
  ;; Enable clipboard integration in the terminal (Emacs 29+)
  ;; This sends the OSC 52 escape sequence automatically when you copy.
  (setq xterm-extra-capabilities '(setSelection)))

(use-package tramp
  :straight nil
  :custom
  (tramp-use-auth-sources nil)
  (auth-source-save-behavior nil)
  (tramp-allow-unsafe-temporary-files t))

(use-package password-cache
  :custom
  (password-cache-expiry 600))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package files
  :straight nil
  :custom
  (backup-by-copying t)       ;; Don't clobber symlinks
  (version-control t)         ;; Use version numbers for backups (foo.txt.~1~)
  (delete-old-versions t)     ;; Delete excess backups silently
  (kept-new-versions 6)       ;; Keep last 6 new versions
  (kept-old-versions 2))      ;; Keep first 2 original versions

(use-package misc
  :straight nil  ;; Built-in library
  :bind
  ;; Rebind M-z from standard 'zap-to-char' to 'zap-up-to-char'
  ("M-z" . zap-up-to-char))

(when (file-exists-p custom-file)
  (load custom-file))
