;;; lisp/google-utils.el --- Google specific helpers -*- lexical-binding: t; -*-

(require 'google)

(defun my/corp-magit-dispatch (orig-fun &rest args)
  (interactive)
  "If inside a Google repo, run fig-status. Otherwise, run normal magit-status."
  (if (and (boundp 'default-directory)
           (string-prefix-p "/google/src" default-directory))
      (call-interactively 'fig-status)
    (apply orig-fun args)))

;; Activate the advice
(advice-add 'magit-status :around #'my/corp-magit-dispatch)

(provide 'corp-utils)
