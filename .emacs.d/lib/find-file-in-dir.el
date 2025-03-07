;;; find-file-in-dir.el

;; Copyright (C) 2015 Adam Spiers

;; Author: Adam Spiers <emacs@adamspiers.org>
;; Maintainer: Adam Spiers <emacs@adamspiers.org>
;; Created: 3 Jan 2015
;; Package-Requires: ((counsel))
;; Keywords: dotemacs file find speed config package
;; URL: https://github.com/aspiers/emacs

(autoload 'counsel--find-file-1 "counsel")

;;;###autoload
(defmacro define-find-file-in-dir-function (name dir &optional prompt)
  "Defines function which invokes equivalent of `counsel-find-file'
  from within a given directory."
  `(defun ,name ()
     ,(format "Uses counsel to find a file within %s.
This function was defined via `define-find-file-in-dir-function',
and invokes `counsel--find-file-1'." dir)
     (interactive)
     (counsel--find-file-1 ,(or prompt "Find file: ")
                           ,dir #'counsel-find-file-action
                           'counsel-find-file)))

(provide 'find-file-in-dir)
