;;; xtla-emacs.el --- Compatibility stuff for stable version of GNU Emacs

;; Copyright (C) 2004 by Stefan Reichoer

;; This file is part of xtla.
;;
;; xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; GNU Emacs is a creature; it grows day by day.  Some part of xtla is
;; based on a CVS version of GNU Emacs.  Therefore xtla didn't work
;; well on the stable version of GNU Eamcs(21.x). This file provides
;; functions making Xtla running on the stable version of GNU Emacs.
;; The most of all code comes from the CVS version of GNU Emacs.

;;; Code:

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

(provide 'xtla-emacs)
;; Local Variables:
;; arch-tag: 66b92889-1ce9-4c1d-818a-8bd5ee499091
;; End:

;;; xtla-emacs.el ends here

