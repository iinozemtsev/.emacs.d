;;; slate-config-mode.el --- A major mode to edit .slate file to config Slate window manager
;;
;; Filename: slate-config-mode.el
;; Description: A major mode to edit .slate file to config Slate window manager
;; Author: Dajun Duan
;; Maintainer: Dajun Duan
;; Created: Sat Dec  1 13:56:34 2012 (-0500)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar slate-config-mode-hook nil)

(defconst slate-config-constants
  '("screenOriginX" "screenOriginY" "screenSizeX" "screenSizeY"
    "windowTopLeftX" "windowTopLeftY" "windowSizeX" "windowSizeY"
    "newWindowSizeX" "newWindowSizeY" "windowHintsWidth" "windowHintsHeight"))
(defconst slate-config-functions
  '("sum" "count" "min" "max" "average" "median" "stddev" "sqrt" "log" "ln" "exp"
    "floor" "ceiling" "abs" "trunc" "random" "randomn"))
(defconst slate-config-config-options
  '("defaultToCurrentScreen" "nudgePercentOf" "resizePercentOf" "repeatOnHoldOps"
    "secondsBeforeRepeat" "secondsBetweenRepeat" "checkDefaultsOnLoad" "focusCheckWidth"
    "focusCheckWidthMax" "focusPreferSameApp" "orderScreensLeftToRight" "windowHintsFontName"
    "windowHintsFontSize" "windowHintsFontColor" "windowHintsWidth" "windowHintsHeight"
    "windowHintsBackgroundColor" "windowHintsDuration" "windowHintsRoundedCornerSize"
    "windowHintsIgnoreHiddenWindows" "windowHintsTopLeftX" "windowHintsTopLeftY"
    "windowHintsOrder" "windowHintsShowIcons" "windowHintsSpread" "windowHintsSpreadSearchWidth"
    "windowHintsSpreadSearchHeight" "windowHintsSpreadPadding" "switchIconSize" "switchIconPadding"
    "switchBackgroundColor" "switchSelectedBackgroundColor" "switchSelectedBorderColor"
    "switchSelectedBorderSize" "switchRoundedCornerSize" "switchOrientation" "switchSecondsBeforeRepeat"
    "switchSecondsBetweenRepeat" "switchStopRepeatAtEdge" "switchOnlyFocusMainWindow" "switchFontSize"
    "switchFontColor" "switchFontName" "switchShowTitles" "switchType" "switchSelectedPadding"
    "keyboardLayout" "snapshotTitleMatch" "snapshotMaxStackSize" "undoMaxStackSize" "undoOps"
    "gridBackgroundColor" "gridRoundedCornerSize" "gridCellBackgroundColor" "gridCellSelectedColor"
    "gridCellRoundedCornerSize" "layoutFocusOnActivate")
  "Keys used in config directive")
(defconst slate-config-operations
  '("move" "resize" "push" "nudge" "throw" "corner" "shell" "hide" "show" "toggle"
    "chain" "sequence" "layout" "focus" "snapshot" "delete-snapshot" "activate-snapshot"
    "hint" "grid" "relaunch" "undo" "switch"))
(defconst slate-config-bind-modifiers
  '("ctrl" "alt" "cmd" "shift" "fn"))
(defconst slate-config-misc-keywords
  '("IGNORE_FAIL" "REPEAT" "MAIN_FIRST" "MAIN_LAST" "SORT_TITLE" "TITLE_ORDER"
    "top-left" "top-right" "bottom-left" "bottom-right" "top" "up" "bottom" "down"
    "left" "right" "none" "center" "bar" "bar-resize" "wait" "path"
    "above" "behind" "save-to-disk" "stack" "all" "delete" "padding" "if_exists"))

;; syntax table
(defvar slate-config-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\s "-" st)
    st)
  "Syntax table for slate mode")

(defvar slate-config-font-lock-keywords
  `(("^\\s-*\\<\\(config\\)\\>"
     (1 font-lock-keyword-face)
     (,(regexp-opt slate-config-config-options 'words) nil nil (0 font-lock-constant-face)))
    ("^\\s-*\\<\\(default\\)\\>"
     (1 font-lock-keyword-face)
     ("\\<count\\|resolutions\\>" nil nil (0 font-lock-constant-face)))
    ("^\\s-*\\<\\(alias\\)\\>\\s-+\\_<\\(\\(\\s_\\|\\w\\)*\\)\\_>"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("^\\s-*\\<\\(layout\\|bind\\|source\\)\\>" 1 font-lock-keyword-face)
    ("\\<throw\\|corner\\>" (0 font-lock-builtin-face) ("\\<resize\\>" nil nil (0 font-lock-keyword-face)))
    ("\\${\\_<\\(\\s_\\|\\w\\)*\\_>}" . font-lock-variable-name-face)
    (,(regexp-opt slate-config-misc-keywords 'words) . font-lock-constant-face)
    (,(regexp-opt (append '() slate-config-functions slate-config-operations) 'words) . font-lock-builtin-face)
    (,(regexp-opt (append '() slate-config-constants slate-config-bind-modifiers) 'words) . font-lock-constant-face)))

(define-derived-mode slate-config-mode fundamental-mode "Slate"
  "Slate mode is a major mode for editing .slate file to config Slate window manager"
  :syntax-table slate-config-mode-syntax-table
  (setq font-lock-defaults '(slate-config-font-lock-keywords)))

(setq auto-mode-alist
      (append '(("\\.slate\\'" . slate-config-mode)) auto-mode-alist))

(provide 'slate-config-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; slate-config-mode.el ends here
