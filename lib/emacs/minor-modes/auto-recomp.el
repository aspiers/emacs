;;; auto-recomp.el --- Automatically recompile Emacs Lisp files

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author:   Michael Shulman <viritrilbia@kurukshetra.cjb.net>
;; Keywords: 

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

;; This file allows you to set up certain emacs lisp files to be
;; automatically recompiled whenever they are saved. 

;;; Code:

(defvar auto-recompile nil
  "Automatically byte-recompile this file whenever it is saved.")
(make-variable-buffer-local 'auto-recompile)

(defun auto-recompile-file-maybe ()
  (when auto-recompile
    (byte-compile-file buffer-file-name)))

(defun add-after-save-hook ()
  (make-local-hook 'after-save-hook)
  (add-hook 'after-save-hook 'auto-recompile-file-maybe))

(add-hook 'emacs-lisp-mode-hook 'add-after-save-hook)

(provide 'auto-recomp)

;;; Local Variables:
;;; auto-recompile: t
;;; End:

;;; auto-recomp.el ends here
