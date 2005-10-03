;;; planner-lisp.el --- Interactive Emacs Lisp integration for Planner mode

;; Copyright (C) 2004 Sandra Jean Chua

;; Author: Sacha Chua <sacha@free.net.ph>
;; Version: 2005.08.20-17.59-stable
;; Keywords: planner, gnus
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This file allows you to launch Emacs Lisp functions
;; from a planner page.
;;
;; Example:
;; [[lisp:/plan][Plan]] will be rendered as "Plan". Selecting the link
;; will run the plan function interactively.
;;
;; If the link is a lisp form it is evaluated non-interactively:
;; [[lisp:/(man "bash")][bash manual]] will be rendered as "bash
;; manual".  Selecting the link will show the bash man page in Emacs. 

;;; Code:

(require 'planner)

;;;###autoload
(defun planner-lisp-browse-url (url)
  "If this is a LISP URL, jump to it."
  (when (string-match "^lisp:/*\\(.+\\)" url)
    (let ((form (match-string 1 url)))
      (if (string-match "\\`\\s-*\\((.+)\\)\\s-*\\'" form)
	  (eval (read (match-string 1 form)))
	(eval (read (concat "(call-interactively '" form ")")))))
    t))

(defun planner-lisp-resolve-url (id)
  "Replace ID with nothing."
  "")

(planner-add-protocol "lisp" 'planner-lisp-browse-url nil)

(provide 'planner-lisp)

;;; planner-lisp.el ends here
