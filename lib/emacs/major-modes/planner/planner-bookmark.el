;;; planner-bookmark.el --- bookmark URL support for the Emacs planner
;;

;; Keywords: emacs planner bookmark remember note
;; Author: Dryice Liu <dryice AT liu DOT com DOT cn>
;; Description: use bookmark.el in Emacs planner

;; This file is not part of GNU Emacs.

;; Copyright (C) 2004 Dryice Dong Liu . All rights reserved.

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
;;
;; Place planner-bookmark.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-bookmark)
;;
;; Annotations will be of the form
;; [[bookmark://bookmark-name][bookmark-description]]
;; bookmark-description will use bookmark-annotation if available,
;; else bookmark-name will be used.
;;
;; Note this file advice `bookmark-set'. If you don't want to take a
;; note everytime you set a bookmark, set
;; `planner-bookmark-take-note-after-set-bookmark-flag' to nil

;;; CODE

(require 'planner)
(require 'bookmark)

;;; User variables

;;; Code:

(defgroup planner-bookmark nil
  "Bookmark URL support for planner.el."
  :prefix "planner-bookmark"
  :group 'planner)

(defcustom planner-bookmark-take-note-after-set-bookmark-flag
  t
  "Non-nil means show a `remember' buffer after setting a new bookmark."
  :type 'boolean
  :group 'planner-bookmark)

(defcustom planner-bookmark-add-note-title-flag
  t
  "Non-nil means add the bookmark name as the default note title"
  :type 'boolean
  :group 'planner-bookmark)

;;;; User variables stop here

(defadvice bookmark-set (after planner-bookmark activate)
  "Display a `remember' buffer for the bookmark.
This code is run only if
`planner-bookmark-take-note-after-set-bookmark-flag' is non-nil."
  (if (and planner-bookmark-take-note-after-set-bookmark-flag
           (condition-case nil
               (require 'remember)
             ('file-error nil)))
      ;; bookmark can take us where we want. we don't need two URLs
      (let ((remember-annotation-functions nil))
        (remember (concat 
		   (if planner-bookmark-add-note-title-flag
		       bookmark-current-bookmark)
		   "\n\n" (planner-bookmark-make-url
			   bookmark-current-bookmark))))))

;;;###autoload
(defun planner-bookmark-annotation-from-bookmark ()
  "If called from a bookmark buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (if (and (eq major-mode 'bookmark-bmenu-mode)
	   (bookmark-bmenu-check-position))
      (planner-bookmark-make-url (bookmark-bmenu-bookmark))))

(defun planner-bookmark-make-url (bookmark-name)
  "Make the bookmark URL by given BOOKMARK-NAME."
  (let ((bookmark-annotation (bookmark-get-annotation bookmark-name)))
    (if (string-equal bookmark-annotation "")
	(setq bookmark-annotation nil))
    (planner-make-link
     (concat "bookmark://" bookmark-name)
     (or bookmark-annotation bookmark-name)
     t)))

;;;###autoload
(defun planner-bookmark-browse-url (url)
  "If this is a bookmark URL, jump to it."
  (when (string-match "^bookmark:/?/?\\(.+\\)" url)
    (bookmark-jump (match-string 1 url))
    t))

(planner-add-protocol "bookmark" 'planner-bookmark-browse-url nil)
(add-hook 'planner-annotation-functions 'planner-bookmark-annotation-from-bookmark)
(custom-add-option 'planner-annotation-functions 'planner-bookmark-annotation-from-bookmark)

(provide 'planner-bookmark)

;;; planner-bookmark.el ends here
