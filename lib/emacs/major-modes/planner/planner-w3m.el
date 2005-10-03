;;; planner-w3m.el --- W3M integration for the Emacs Planner

;; Copyright (C) 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Version: 2005.08.20-17.59-stable
;; Keywords: planner, w3m
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php#w3m

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module allows you to create tasks from a w3m buffer.

;;; Code:

(require 'w3m)
(require 'planner)

;;;###autoload
(defun planner-w3m-annotation-from-w3m ()
  "If called from a w3m page, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (eq major-mode 'w3m-mode)
    (planner-make-link w3m-current-url w3m-current-title t)))

(add-hook 'planner-annotation-functions 'planner-w3m-annotation-from-w3m)
(custom-add-option 'planner-annotation-functions
                   'planner-w3m-annotation-from-w3m)

(provide 'planner-w3m)

;;; planner-w3m.el ends here
