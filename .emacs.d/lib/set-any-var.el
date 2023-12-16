;;; set-any-var.el - interactive variable value editor

;; Copyright (C) 2001 Adam Spiers <adam@spiers.net>

;; Author: Adam Spiers <adam@spiers.net>

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'wid-edit)
(require 's)

(defvar set-any-variable-minibuffer-setup-hook nil
  "Hook run by `set-any-variable' when setting up the minibuffer.

This can be used to set the minibuffer to a particular mode such as
lispy-mode for convenient editing of the elisp value.")

(defun set-any-variable (var val)
  "Just like `set-variable', but allows you to set any variable, not just
so-called \`user variables' (ones whose documentation begins with a \`*'
character), and interactively setting the value, provides the current value
as a starting point for convenient editing."
  (interactive
   (let* ((default-var (variable-at-point))
          (enable-recursive-minibuffers t)
          (prompt (if (symbolp default-var)
                      (format
                       "Set variable (default %s): " default-var)
                    "Set variable: "))
          (default (if (symbolp default-var) (symbol-name default-var)))
          (var-in (completing-read
                   prompt
                   obarray 'boundp t nil 'set-any-variable-history
                   default))
          (var (intern (cond ((equal var-in "") default-var)
                             (t var-in))))
          (prop (get var 'variable-interactive))
          (val (if prop
                   ;; Use VAR's `variable-interactive' property
                   ;; as an interactive spec for prompting.
                   (call-interactively `(lambda (arg)
                                          (interactive ,prop)
                                          arg))
                 (read
                  (minibuffer-with-setup-hook
                      (lambda ()
                        (run-hooks 'set-any-variable-minibuffer-setup-hook))
                    (read-string (format "Set %s to: " var)
                                 (string-trim
                                  (pp (symbol-value var))
                                  nil "\n")
                                 'set-variable-value-history))))))
     (list var val)))

  (let ((type (get var 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'wid-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match val)
        (error "Value `%S' does not match type %S of %S"
               val (car type) var))))
  (set var val)

  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

(defun set-env-variable (var val)
  "Interactively set an environment variable."
  (interactive
   (let* ((at-point (thing-at-point 'symbol))
          (default-var (if (and (stringp at-point)
                                (s-uppercase-p at-point))
                           at-point "PATH"))
          (enable-recursive-minibuffers t)
          (prompt (if default-var
                      (format "Set environment variable (default %s): " default-var)
                    "Set environment variable: "))
          (env-vars (mapcar
                     (lambda (x)
                       (replace-regexp-in-string "=.*" "" x))
                     process-environment))
          (var (completing-read
                   prompt
                   env-vars 'stringp t nil 'set-env-variable-history
                   default-var))
          (val (read-string (format "Set %s to: " var)
                            (getenv var)
                            nil ""
                            'set-variable-value-history)))
     (list var val)))

  (setenv var val))

(provide 'set-any-variable)

;;; set-any-var.el ends here
