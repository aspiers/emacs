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

(defun set-any-variable (var val)
  "Just like `set-variable', but allows you to set any variable, not just
so-called \`user variables' (ones whose documentation begins with a \`*'
character), and interactively setting the value, provides the current value
as a starting point for convenient editing."
  (interactive
   (let* ((default-var (variable-at-point))
          (enable-recursive-minibuffers t)
          (var-in (completing-read
                   (if (symbolp default-var)
                       (format
                        "Set variable (default %s): " default-var)
                     "Set variable: ")
                   obarray 'boundp t nil nil
                   (if (symbolp default-var) (symbol-name default-var))))
          (var (intern (cond ((equal var-in "") default-var)
                             (t var-in))))
          (prop (get var 'variable-interactive))
          (val (if prop
                   ;; Use VAR's `variable-interactive' property
                   ;; as an interactive spec for prompting.
                   (call-interactively `(lambda (arg)
                                          (interactive ,prop)
                                          arg))
                 (read (read-string (format "Set %s to: " var)
                                    (pp (symbol-value var))
                                    'set-variable-value-history)))))
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

(provide 'set-any-variable)

;;; set-any-var.el ends here
