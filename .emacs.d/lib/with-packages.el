;;; with-packages.el --- multi-package config     -*- lexical-binding: t -*-

;; Copyright (C) 2020 Adam Spiers

;; Author: Adam Spiers <emacs@adamspiers.org>
;; Created: 1 Jul 2020
;; Homepage: https://github.com/aspiers/emacs
;; Keywords: extensions
;; Package-Requires: ((use-package "0"))
;; SPDX-License-Identifier: MIT
;; Version: prerelease

;;; Introduction

;; The popular `use-package' is great for setting up deferred loading
;; of packages, and associating configuration to be activated when
;; individual packages are lazy-loaded, but it's not so convenient
;; when it is required to trigger configuration only after /multiple/
;; packages are loaded.
;;
;; So this package provides an extension to `use-package' making that
;; easy.

;;; Example usage

;; For example, imagine you had org-mode and counsel both set up for
;; lazy loading in order to minimize your emacs start time.  The
;; following binding would only be set up after both packages are
;; loaded.
;;
;;   (with-packages (org counsel)
;;     :bind (:map org-mode-map
;;                 (("C-c C-j" . counsel-org-goto))))

;;; History

;; See https://github.com/jwiegley/use-package/issues/71 where this
;; functionality was originally proposed and implementations

;;; Dependencies

(require 'use-package)

;;; Code

(defmacro with-packages (when &rest args)
  "Allow declaration of `use-package' config which is only
triggered when a required set of packages are loaded.  The
required set is defined by the `when' argument, whose value is
exactly the same format as with the `:after' argument to
`use-package'.

The `args' are passed straight to `use-package' for use as normal.

Example usage:

  (with-packages (org counsel)
    :bind (:map org-mode-map
                ((\"C-c C-j\" . counsel-org-goto))))"
  (let ((pseudo-pkg-name
         (intern (format
           "*with-packages/%s"
           (let ((s (prin1-to-string when)))
             (dolist (repl '(("(\\(.+\\))" . "\\1")
                             (" " . "-")
                             (":any" . "any")
                             (":all" . "all"))
                           s)
               (let ((from (car repl))
                     (to (cdr repl)))
                 (setq s (replace-regexp-in-string from to s)))))))))
    `(progn
       ;; (message "Defining pseudo-package %s" ,pseudo-pkg-name)
       (use-package ,pseudo-pkg-name
         :no-require t
         :defer nil
         :ensure nil
         :straight nil
         :after ,when
         ,@args)
       ;; (message "use-package done for %s" ,pseudo-pkg-name)
       )))

(put 'with-packages 'lisp-indent-function 'defun)

;;;; Closing remarks

(provide 'with-packages)

;;; with-packages.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
