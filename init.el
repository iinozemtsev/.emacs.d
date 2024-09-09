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

(use-package magit 
  :bind (("M-g s" . magit-status))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("449a6a03953bf946290f1a0eac09a7de2cf2556aa890f28ad2510a494eb23dcb" "993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" default))
 '(package-selected-packages
   '(vscdark-theme vscode-dark-plus-theme dart-mode ligature window-numbering magit lua-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
