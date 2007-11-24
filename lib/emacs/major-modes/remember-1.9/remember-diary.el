;;; remember-diary --- extracting diary information from buffers

;; Copyright (C) 1999, 2000, 2001 John Wiegley
;; Copyright (C) 2004 Sandra Jean Chua

;; Author: Sacha Chua <sacha@free.net.ph>
;; Created: 24 Mar 2004
;; Keywords: data memory todo pim diary
;; URL: http://gna.org/projects/remember-el/

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

;; This module recognizes entries of the form
;;
;;     DIARY: ....
;;
;; and puts them in your ~/.diary (or remember-diary-file) together
;; with an annotation. Planner-style dates (yyyy.mm.dd) are converted
;; to yyyy-mm-dd so that diary can understand them.
;;
;; For example:
;;
;; DIARY: 2003.08.12 Sacha's birthday
;;
;; is stored as
;;
;; 2003.08.12 Sacha's birthday [[/home/sacha/notebook/emacs/emacs-wiki/remember-diary.el]]
;;
;; To use, add the following to your .emacs:
;;
;;    (require 'remember-diary)
;;    ;; This should be before other entries that may return t
;;    (add-to-list 'remember-handler-functions 'remember-diary-extract-entries)
;;

(require 'remember)
(require 'diary-lib)

;;; Code:
(defcustom remember-diary-file diary-file
  "*File for extracted diary entries."
  :type 'file
  :group 'remember)

(defun remember-diary-convert-entry (entry)
  "Translate MSG to an entry readable by diary."
  (save-match-data
    (when remember-annotation
        (setq entry (concat entry " " remember-annotation)))
    (if (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" entry)
        (replace-match
         (if european-calendar-style
             (concat (match-string 3 entry) "/"
                     (match-string 2 entry) "/"
                     (match-string 1 entry))
           (concat (match-string 2 entry) "/"
                   (match-string 3 entry) "/"
                   (match-string 1 entry)))
         t t entry)
      entry)))

(defun remember-diary-extract-entries ()
  "Extract diary entries from the region."
  (save-excursion
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward "^DIARY:\\s-*\\(.+\\)" nil t)
        (add-to-list 'list (remember-diary-convert-entry (match-string 1))))
      (when list
        (make-diary-entry (mapconcat 'identity list "\n")
                          nil remember-diary-file))
      nil))) ;; Continue processing

(provide 'remember-diary)

;;; remember-diary.el ends here
