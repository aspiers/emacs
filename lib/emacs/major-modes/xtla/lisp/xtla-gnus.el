;;; xtla.el --- Arch interface for emacs

;; Copyright (C) 2003-2005 by Stefan Reichoer

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Contributions from:

;; Xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Gnus integration for Xtla

;;; History

;; Created on August 12, 2005

(require 'xtla-core)

;;;###autoload
(defun tla-gnus-setup-buttons ()
  "Make archive@host.com/something clickable in Gnus Article buffer."
  (interactive)
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 0 t t) 1 t
                 tla-categories-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 1 t t) 1 t
                 tla-branches-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 2 t t) 1 t
                 tla-versions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 3 t t) 1 t
                 tla-revisions-string 1))
  (add-to-list 'gnus-button-alist
               '((tla-make-name-regexp 4 t t) 1 t
                 tla--button-revision-fn 1))
  )

; arch-tag: def5ed46-2cd9-4bf4-b701-fc8f1f4865d0
(provide 'xtla-gnus)

;; xtla-gnus.el ends here
