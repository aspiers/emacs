;;;; psgml-other.el --- Part of SGML-editing mode with parsing support
;; $Id$

;; Copyright (C) 1994 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>

;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;;; Part of psgml.el. Code not compatible with XEmacs.


;;;; Code:

(require 'psgml)
(require 'easymenu)

(defvar sgml-max-menu-size (/ (* (frame-height) 2) 3)
  "*Max number of entries in Tags and Entities menus before they are split
into several panes.")


;;;; Key Commands

;; Doesn't this work in Lucid? ***
(define-key sgml-mode-map [?\M-\C-\ ] 'sgml-mark-element)

(define-key sgml-mode-map [S-mouse-3] 'sgml-tags-menu)


;;;; Pop Up Menus

(defun sgml-popup-menu (event title entries)
  "Display a popup menu.
ENTRIES is a list where every element has the form (STRING . VALUE) or
STRING."
  (x-popup-menu
   event
   (let ((menus (list (cons title entries))))
     (cond
      ((> (length entries) sgml-max-menu-size)
       (setq menus
	     (loop for i from 1 while entries
		   collect
		   (let ((submenu
			  (subseq entries 0 (min (length entries)
						 sgml-max-menu-size))))
		     (setq entries (nthcdr sgml-max-menu-size entries))
		     (cons
		      (format "%s '%s'-'%s'"
			      title
			      (sgml-range-indicator (caar submenu))
			      (sgml-range-indicator (caar (last submenu))))
		      submenu))))))
     (cons title menus))))

(defun sgml-range-indicator (string)
  (substring string
	     0
	     (min (length string) sgml-range-indicator-max-length)))

(defun sgml-popup-multi-menu (event title menus)
  "Display a popup menu.
MENUS is a list of menus on the form (TITLE ITEM1 ITEM2 ...).
ITEM should have to form (STRING EXPR) or STRING.  The EXPR gets evaluated
if the item is selected."
  (nconc menus '(("---" "---")))	; Force x-popup-menu to use two level
					; menu even if there is only one entry
					; on the first level
  (eval (car (x-popup-menu event (cons title menus)))))


;;;; Insert with properties

(defvar sgml-write-protect-intagible
  (not (boundp 'emacs-minor-version)))

(defun sgml-insert (props format &rest args)
  (let ((start (point)))
    (insert (apply (function format)
		   format
		   args))
    (when (and sgml-write-protect-intagible
	       (getf props 'intangible))
	  (setf (getf props 'read-only) t))
    (add-text-properties start (point) props)))


;;;; Set face of markup

(defvar sgml-use-text-properties nil)

(defun sgml-set-face-for (start end type)
  (let ((face (cdr (assq type sgml-markup-faces))))
    (cond
     (sgml-use-text-properties
      (let ((inhibit-read-only t)
	    (after-change-function nil)	; obsolete variable
	    (before-change-function nil) ; obsolete variable
	    (after-change-functions nil)
	    (before-change-functions nil))
	(put-text-property start end 'face face)
        (when (< start end)
          (put-text-property (1- end) end 'rear-nonsticky '(face)))))
     (t
      (let ((current (overlays-at start))
	    (pos start)
	    old-overlay)
	(while current
	  (cond ((and (null old-overlay)
                      type
		      (eq type (overlay-get (car current) 'sgml-type)))
		 (setq old-overlay (car current)))
		((overlay-get (car current) 'sgml-type)
		 ;;(message "delov: %s" (overlay-get (car current) 'sgml-type))
		 (delete-overlay (car current))))
	  (setq current (cdr current)))
	(while (< (setq pos (next-overlay-change pos))
		  end)
	  (setq current (overlays-at pos))
	  (while current
	    (when (overlay-get (car current) 'sgml-type)
	      (delete-overlay (car current)))
	    (setq current (cdr current))))
	(cond (old-overlay
	       (move-overlay old-overlay start end)
	       (if (null (overlay-get old-overlay 'face))
		   (overlay-put old-overlay 'face face)))
	      (face
	       (setq old-overlay (make-overlay start end))
	       (overlay-put old-overlay 'sgml-type type)
	       (overlay-put old-overlay 'face face))))))))

(defun sgml-set-face-after-change (start end &optional pre-len)
  ;; If inserting in front of an markup overlay, move that overlay.
  ;; this avoids the overlay beeing deleted and recreated by
  ;; sgml-set-face-for.
  (when (and sgml-set-face (not sgml-use-text-properties))
    (loop for o in (overlays-at start)
	  do (cond
	      ((not (overlay-get o 'sgml-type)))
	      ((= start (overlay-start o))
	       (move-overlay o end (overlay-end o)))))))

(defun sgml-fix-overlay-after-change (overlay flag start end &optional size)
  (message "sfix(%s): %d-%d (%s)" flag start end size)
  (overlay-put overlay 'front-nonsticky t)
  (when nil
    (move-overlay overlay end (overlay-end overlay))))

(defalias 'next-overlay-at 'next-overlay-change) ; fix bug in cl.el

(defun sgml-clear-faces ()
  (interactive)
  (loop for o being the overlays
	if (overlay-get o 'sgml-type)
	do (delete-overlay o)))


;;;; Emacs before 19.29

(unless (fboundp 'buffer-substring-no-properties)
  (defalias 'buffer-substring-no-properties 'buffer-substring))


;;;; Provide

(provide 'psgml-other)

;;; psgml-other.el ends here
