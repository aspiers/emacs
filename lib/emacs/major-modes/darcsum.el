;;; darcsum.el --- a pcl-cvs like interface for managing darcs patches

;; Copyright (C) 2004  John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: John Wiegley <johnw@gnu.org>
;; Keywords: completion convenience tools vc
;; Version: 1.00
;; location: http://www.newartisans.com/johnw/emacs.html

;; This file is not yet part of GNU Emacs.

;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This module is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Load this file and run M-x darcsum-whatsnew.  This will display a
;; pcl-cvs like buffer showing modified files.  RET on a file reveals
;; changes; RET on a directory reveals changes to its files.
;;
;; Displayed changes may be recorded with "c", which offers a buffer
;; for inputing the change name (first line) and long description
;; (subsequent lines).  C-c C-c records the patch.
;;
;; If a change is "marked" in the summary buffer with "m" (done on the
;; change, the file (all changes) or the directory (all changes in all
;; files)), only marked changes are recorded, regardless of point.
;;
;; Changes can be removed with "r".  Move changes between buffers with
;; "M", which prompts for a darcsum buffer to move to (creating one if
;; the buffer doesn't exist).
;;
;; "g" forgets everything and resubmits the "whatsnew" command.
;; Collapsing a file forgets all marks for that file.  Only displayed
;; changes are ever recorded!
;;
;; "n" and "p" move among files.  "q" kills the buffer.

;; TODO:

;; - When merging changesets, check the content of change text too
;; - Better support for moving files
;; - use --interactive with apply, for applying patches from e-mail
;;   via darcsum

;;; Code:

(defgroup darcsum nil
  "Special support for the Darcs versioning system."
  :version "21.4"
  :group 'tools
  :prefix "darcsum-")

(defvar darcsum-data nil)
(defvar darcsum-look-for-adds nil)

(defface darcsum-header-face
  '((((class color) (background dark))
     (:foreground "lightyellow" :weight bold))
    (((class color) (background light))
     (:foreground "blue4" :weight bold))
    (t (:weight bold)))
  "PCL-CVS face used to highlight directory changes."
  :group 'darcsum)

(defface darcsum-marked-face
  '((t (:weight bold)))
  "Face used to highlight marked changes."
  :group 'darcsum)

(defface darcsum-need-action-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:slant italic)))
  ""
  :group 'darcsum)

(defface darcsum-need-action-marked-face
  '((((class color) (background dark))
     (:foreground "orange" :bold t))
    (((class color) (background light))
     (:foreground "orange" :bold t))
    (t (:slant italic :bold t)))
  ""
  :group 'darcsum)

(defface darcsum-filename-face
  '((((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "blue4"))
    (t ()))
  "PCL-CVS face used to highlight file names."
  :group 'darcsum)

(defface darcsum-change-line-face
  '((((class color) (background dark))
     (:foreground "grey75" :background "grey25"))
    (((class color) (background light))
     (:foreground "grey25" :background "grey75"))
    (t (:weight bold)))
  "PCL-CVS face used to highlight file names."
  :group 'darcsum)

(defun darcsum-add-props (str &rest props)
  (add-text-properties 0 (1- (length str)) (list* props) str)
  str)

(defun darcsum-add-face (str face &optional keymap &rest props)
  (when keymap
    (when (keymapp keymap)
      (setq props (list* 'keymap keymap props)))
    (setq props (list* 'mouse-face 'highlight props)))
  (add-text-properties 0 (length str) (list* 'font-lock-face face props) str)
  str)

;;; Code to work with changesets

;; A changeset is an alist of the following form:
;;
;;   ((DIR (FILE (LINE CHANGE...))))
;;
;; Where DIR and FILE are plain strings, but LINE is of the following
;; possible formats:
;;
;;   LINE     An integer giving the first line of the change
;;   -LINE    Integer line of change, but change is not "visible"
;;   (LINE)   Integer line of change, but change is "marked"
;;
;; Each CHANGE is a string which represent a modification to make to
;; the file after the starting LINE.  It begins with either a "+" or
;; "-" to indicate if the line should be removed or added to the file.
;;
;; So, for example, in a buffer with no changes visible yet:
;;
;; (("."
;;  ("TODO" addfile)
;;  ("report.cc"
;;   (-606 "-    blah" "+    blah" "+    blah")
;;   (-620 "-    blah" "+    blah" "+    blah")
;;   (-629 "-    blah" "+    blah" "+    blah")
;;   (-634 "-    blah" "+    blah" "+    blah")
;;   (-641 "-    blah" "+    blah" "+    blah")
;;   (-652 "-    blah" "+    blah" "+    blah")
;;   (-664 "-    blah" "+    blah" "+    blah"))
;;  ("report.h"
;;   (-115 "-    blah" "+    blah" "+    blah")
;;   (-126 "+"))))

(if (featurep 'xemacs)
    ;; make-temp-name generates a unique name when it is called, but
    ;; takes no provisions to ensure that it will remain unique. Thus,
    ;; there is a race condition before we use the name. This is
    ;; probably a bad thing.
    (defalias 'darcsum-make-temp-file 'make-temp-name)
  (defalias 'darcsum-make-temp-file 'make-temp-file))

(defsubst darcsum-change-item (change)
  (if (listp (car change))
      (caar change)
    (car change)))

(defsubst darcsum-change-line (change)
  (let ((ch (darcsum-change-item change)))
    (if (symbolp ch)
	1
      ch)))

(defun darcsum-applicable-p (data predicate)
  (catch 'exit
    (ignore
     (let (dir file change)
       (dolist (dir data)
	 (dolist (file (cdr dir))
	   (dolist (change (cdr file))
	     (if (funcall predicate (car dir) (car file) change)
		 (throw 'exit t)))))))))

(defsubst darcsum-marked-p (data)
  (darcsum-applicable-p data (function
			      (lambda (dir file change)
				(listp (car change))))))

(defsubst darcsum-changeset-has-change-p (data odir ofile start-line)
  (darcsum-applicable-p
   data (function
	 (lambda (d f change)
	   (and (equal odir d)
		(equal ofile f)
		(if (symbolp start-line)
		    (eq start-line (darcsum-change-item change))
		  (equal start-line
			 (abs (darcsum-change-line change)))))))))

(defun darcsum-find-changeset (data predicate)
  (let (dir file change changeset)
    (dolist (dir data)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (if (funcall predicate (car dir) (car file) change)
	      (setq changeset
		    (darcsum-add-changeset
		     changeset
		     (list (list (car dir) (list (car file) change)))))))))
    changeset))

(defun darcsum-apply-to-changeset (data func)
  (let (dir file change)
    (dolist (dir data)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (funcall func (car dir) (car file) change))))))

(defun darcsum-remove-changeset (data changeset)
  "Remove DATA from the current changeset."
  (let (dir file change)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (let* ((dentry (assoc (car dir) data))
		 (fentry (assoc (car file) (cdr dentry))))
	    (setcdr fentry (delete (assoc (car change) (cdr fentry))
				   (cdr fentry)))
	    (unless (cdr fentry)
	      (setcdr dentry (delete fentry (cdr dentry))))
	    (unless (cdr dentry)
	      (setq data (delete dentry data))))))))
  data)

(defun darcsum-add-changeset (data changeset)
  "Add DATA to the current changeset."
  (let (dir file change)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (let ((dentry (assoc (car dir) data)))
	    (if dentry
		(let ((fentry (assoc (car file) dentry)))
		  (if fentry
		      (unless (member change (cdr fentry))
			(nconc fentry (list change))
			(setcdr fentry
				(sort (cdr fentry)
				      (function
				       (lambda (l r)
					 (< (abs (darcsum-change-line l))
					    (abs (darcsum-change-line r))))))))
		    (nconc dentry (list (list (car file) change)))))
	      (setq data (cons (list (car dir)
				     (list (car file) change))
			       data))))))))
  data)

(defun darcsum-merge-changeset (data changeset)
  "Merge DATA into the current changeset."
  (let (dir file change final-data)
    (dolist (dir changeset)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (let ((dentry (assoc (car dir) data)))
	    (if dentry
		(let ((fentry (assoc (car file) dentry))
		      (item (darcsum-change-item change)))
		  (if fentry
		      (unless
			  (if (integerp item)
			      (or (assoc item (cdr fentry))
				  (assoc (- item) (cdr fentry))
				  (assoc (list item) (cdr fentry)))
			    (or (assoc item (cdr fentry))
				(assoc (list item) (cdr fentry))))
			(nconc fentry (list change))
			(setcdr fentry
				(sort (cdr fentry)
				      (function
				       (lambda (l r)
					 (< (abs (darcsum-change-line l))
					    (abs (darcsum-change-line r))))))))
		    (nconc dentry (list (list (car file) change)))))
	      (setq data (cons (list (car dir)
				     (list (car file) change))
			       data)))))))
    (dolist (dir data)
      (dolist (file (cdr dir))
	(dolist (change (cdr file))
	  (let* ((dentry (assoc (car dir) changeset))
		 (fentry (assoc (car file) dentry))
		 (item (darcsum-change-item change))
		 final-dentry final-fentry)
	    (when (and dentry fentry
		       (if (integerp item)
			   (or (assoc item (cdr fentry))
			       (assoc (- item) (cdr fentry))
			       (assoc (list item) (cdr fentry)))
			 (or (assoc item (cdr fentry))
			     (assoc (list item) (cdr fentry)))))
	      (unless (setq final-dentry (assoc (car dir) final-data))
		(setq final-data (cons (list (car dir)) final-data)
		      final-dentry (assoc (car dir) final-data)))
	      (unless (setq final-fentry (assoc (car file) final-dentry))
		(nconc final-dentry (list (list (car file))))
		(setq final-fentry (assoc (car file) final-dentry)))
	      (nconc final-fentry (list change)))))))
    (nreverse final-data)))

(defun darcsum-parse-changeset (&optional pending visible)
  "Return the patch in the current buffer as a Lisp changeset."
  (forward-line)
  (let ((limit (* 10 (count-lines (point-min) (point-max))))
	data entries ignore-next-hunk)
    (while (and (not (or (eobp) (looking-at "^}")))
		(> limit 0))
      (setq limit (1- limit))
      (cond
       ((looking-at "^adddir\\s-+\\(.+?\\)$")
	(forward-line))
       ((looking-at "^rmdir\\s-+\\(.+?\\)$")
	(forward-line))
       ((looking-at "^move\\s-+\\(.+?\\)$")
	(forward-line))
       ((looking-at "^binary\\s-+\\(.+?\\)$")
	(forward-line))
       ((looking-at "^\\(addfile\\|rmfile\\|hunk\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\)\\)?$")
	(let* ((kind (match-string 1))
	       (file (match-string 2))
	       (dir (directory-file-name (file-name-directory file)))
	       (base (file-name-nondirectory file))
	       (start-line (match-string 4))
	       (ignore-hunk (and ignore-next-hunk
				 (string= kind "hunk")))
	       lines)
	  (if ignore-next-hunk
	      (setq ignore-next-hunk nil))
	  (forward-line)
	  (when start-line
	    (while (looking-at "^\\([+-].*\\)")
	      (unless ignore-hunk
		(setq lines (cons (match-string 1) lines)))
	      (forward-line)))
	  (unless ignore-hunk
	    (cond
	     ((string= kind "addfile")
	      (let ((add-dir dir)
		    (add-file base))
		(if (or (eq pending t)
			(and pending
			     (darcsum-find-changeset
			      pending
			      (function
			       (lambda (pdir pfile pchange)
				 (and (string= add-dir pdir)
				      (string= add-file pfile)))))))
		    (setq entries (list 'addfile))
		  (setq entries (list 'newfile))))
	      (setq ignore-next-hunk t))
	     ((string= kind "rmfile")
	      (setq entries (list 'rmfile)
		    ignore-next-hunk t))
	     (t
	      (assert (string= kind "hunk"))
	      (setq entries (cons (if visible
				      (string-to-int start-line)
				    (- (string-to-int start-line)))
				  (nreverse lines)))))
	    (let ((entry (assoc dir data)))
	      (if (null entry)
		  (setq data
			(cons (cons dir (list (cons base
						    (list entries)))) data))
		(if entry
		    (let ((item (assoc base entry)))
		      (if item
			  (nconc item (list entries))
			(nconc entry
			       (list (cons base (list entries))))))))))))))
    (assert (> limit 0))
    (nreverse data)))

(defun darcsum-read-changeset (&optional visible)
  (let ((pending
	 (if (file-readable-p "_darcs/patches/pending")
	     (with-temp-buffer
	       (insert-file-contents "_darcs/patches/pending")
	       (darcsum-parse-changeset t)))))
    (goto-char (point-min))
    (if (looking-at "^{")
	(darcsum-parse-changeset pending visible))))

(defun darcsum-display-changeset (data)
  "display the changeset data using a pcl-cvs-like buffer."
  (erase-buffer)
  ;;(when (file-readable-p "_darcs/prefs/lastrepo")
  ;;  (insert "repository : ")
  ;;  (insert-file-contents "_darcs/prefs/lastrepo")
  ;;  (goto-char (point-max)))
  (insert "Working dir: " default-directory "\n\n\n")
  (unless data
    (insert "there are no changes to review.\n"))
  (let (dir file change line)
    (dolist (dir data)
      (insert
       (darcsum-add-props
	(concat "in directory "
		(darcsum-add-face (concat (car dir))
				  'darcsum-header-face t)
		":\n")
	'darcsum-line-type 'dir
	'darcsum-dir (car dir)))
      (dolist (file (cdr dir))
	(let* ((first-change (darcsum-change-item (cadr file)))
	       (all-marked (listp (car (cadr file))))
	       (status
		(cond
		 ((eq first-change 'newfile) "New")
		 ((eq first-change 'addfile) "Added")
		 ((eq first-change 'rmfile) "Removed")
		 (t "Modified"))))
	  (when (string= status "Modified")
	    (setq all-marked t)
	    (dolist (change (cdr file))
	      (if (and all-marked
		       (not (listp (car change))))
		  (setq all-marked nil))))
	  (insert
	   (darcsum-add-props
	    (concat "              "
		    (darcsum-add-face (format "%-24s" status)
				      (if all-marked
					  'darcsum-need-action-marked-face
					'darcsum-need-action-face) t)
		    (darcsum-add-face (concat (car file))
				      'darcsum-filename-face t) "\n")
	    'darcsum-line-type 'file
	    'darcsum-dir (car dir)
	    'darcsum-file (car file))))
	(dolist (change (cdr file))
	  (when (and (not (symbolp (darcsum-change-item change)))
		     (> (darcsum-change-line change) 0))
	    (let ((beg (point)))
	      (insert
	       (darcsum-add-face
		(format "%-10d" (darcsum-change-line change))
		'darcsum-change-line-face t) ?\n)
	      (dolist (line (cdr change))
		(insert (if (not (listp (car change)))
			    line
			  (darcsum-add-face (concat line)
					    'darcsum-marked-face t))
			?\n))
	      (add-text-properties beg (point)
				   (list 'darcsum-line-type 'change
					 'darcsum-dir (car dir)
					 'darcsum-file (car file)
					 'darcsum-change change))))))))
  (insert "
--------------------- End ---------------------\n"))

;;; Code to determine the current changeset in darcsum-mode

(defun darcsum-changeset-at-point (&optional invisible-too)
  (let* ((type (get-text-property (point) 'darcsum-line-type))
	 (dir (get-text-property (point) 'darcsum-dir))
	 (dentry (and dir (assoc dir darcsum-data)))
	 data)
    (cond
     ((eq type 'dir)
      (setq data (list dentry)))
     ((eq type 'file)
      (let* ((file (get-text-property (point) 'darcsum-file))
	     (fentry (assoc file dentry)))
	(setq data (list (list (car dentry) fentry)))))
     ((eq type 'change)
      (let* ((file (get-text-property (point) 'darcsum-file))
	     (fentry (assoc file dentry)))
	(setq data (list
		    (list (car dentry)
			  (list (car fentry)
				(get-text-property (point)
						   'darcsum-change))))))))
    (if invisible-too
	data
      (darcsum-find-changeset data
			      (function
			       (lambda (dir file change)
				 (setq change (darcsum-change-item change))
				 (or (symbolp change) (>= change 0))))))))

(defun darcsum-selected-changeset (&optional all-visible)
  "Return the currently selected changeset.
If marks are active, always returned the marked changes.
Otherwise, return the changes related to point, unless VISIBLE is
non-nil, in which case return all visible changes."
  (cond
   ((darcsum-marked-p darcsum-data)
    (darcsum-find-changeset darcsum-data
			    (function
			     (lambda (dir file change)
			       (listp (car change))))))
   (all-visible
    (darcsum-find-changeset darcsum-data
			    (function
			     (lambda (dir file change)
			       (and (numberp (car change))
				    (>= (car change) 0))))))
   (t
    (darcsum-changeset-at-point))))

;;; Code to record the current changeset

;; If there are any marked changes, these are what get recorded.
;; Otherwise, all *visible* changes are recorded.

(defcustom darcsum-comment-mode-hook nil
  "*Functions run upon entering darcsum-comment-mode."
  :type 'hook
  :group 'darcsum)

(defcustom darcsum-register ?S
  "The register in which the window configuration is stored."
  :type 'character
  :group 'darcsum)

(defcustom darcsum-program "darcs"
  "*The program which darcsum will use to invoke darcs."
  :type 'string
  :group 'darcsum)

(defcustom darcsum-default-expanded nil
  "*If non-nil, the *darcsum* buffer will be expanded by default."
  :type 'boolean
  :group 'darcsum)

(defvar darcsum-process-arg nil)
(defvar darcsum-parent-buffer nil)
(defvar darcsum-changeset-to-record nil)
(defvar darcsum-logfile)

(defsubst darcsum-changes-handled ()
  (if (buffer-live-p darcsum-parent-buffer)
      (let ((changeset darcsum-changeset-to-record))
	(with-current-buffer darcsum-parent-buffer
	  (setq darcsum-data
		(darcsum-remove-changeset darcsum-data changeset))
	  (darcsum-refresh)))))

(defun darcsum-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((looking-at "\n*Finished recording patch")
	(message "Changes recorded.")
	(darcsum-changes-handled)
	(delete-file darcsum-logfile)
	(kill-buffer (current-buffer)))
       ((looking-at "\n*Ok, if you don't want to record anything")
	(message "No changes recorded.")
	(delete-file darcsum-logfile)
	(kill-buffer (current-buffer)))

       ((looking-at "\n*What is the target email address")
	(process-send-string proc darcsum-process-arg)
	(delete-region (point-min) (point-max)))
       ((looking-at "\n*Successfully sent patch bundle")
	(message "Changes sent to '%s'." darcsum-process-arg)
	(kill-buffer (current-buffer)))
       ((looking-at "\n*You don't want to send any patches")
	(message "No changes sent.")
	(kill-buffer (current-buffer)))

       ((looking-at "\n*Do you really want to do this\\? ")
	(process-send-string proc "y\n")
	(delete-region (point-min) (point-max)))
       ((looking-at "\n*Finished reverting.")
	(message "Changes reverted.")
	(darcsum-changes-handled)
	(kill-buffer (current-buffer)))
       ((looking-at "\n*If you don't want to revert")
	(message "No changes reverted.")
	(kill-buffer (current-buffer)))

       ((looking-at "\n*\\(addfile\\|rmfile\\|hunk\\)\\s-+\\(.+?\\)\\(\\s-+\\([0-9]+\\)\\)?$")
	(let* ((kind (match-string 1))
	       (file (match-string 2))
	       (dir (directory-file-name
		     (file-name-directory file)))
	       (base (file-name-nondirectory file))
	       (start-line (if (match-string 4)
			       (string-to-int (match-string 4))
			     (intern kind)))
	       (record (darcsum-changeset-has-change-p
			darcsum-changeset-to-record dir base start-line)))
	  (goto-char (match-end 0))
	  (forward-line)
	  (while (looking-at "^\\([+-].*\\)")
	    (forward-line))
	  (when (looking-at
		 "^Shall I \\(record\\|send\\|revert\\) this patch\\?.+\\] ")
	    (let ((end (match-end 0)))
	      (process-send-string proc (if record "y\n" "n\n"))
	      (delete-region (point-min) end)))))

       ((looking-at "\n*\\(move\\).+")
	(goto-char (match-end 0))
	(forward-line)
	(when (looking-at
	       "^Shall I \\(record\\|send\\|revert\\) this patch\\?.+\\] ")
	  (let ((end (match-end 0)))
	    (process-send-string proc "n\n")
	    (delete-region (point-min) end))))))))

(defun darcsum-really-record ()
  (interactive)
  (let ((buf (generate-new-buffer " *darcs record*"))
	(tempfile (darcsum-make-temp-file "darcsum"))
	(parent-buf darcsum-parent-buffer)
	(changeset darcsum-changeset-to-record))
    (goto-char (point-max))
    (unless (bolp)
      (insert ?\n))
    (write-region (point-min) (point-max) tempfile)
    (kill-buffer (current-buffer))
    (jump-to-register darcsum-register)
    (message "Recording changes...")
    (let ((proc (start-process "darcs" buf
			       darcsum-program "record" "--logfile" tempfile)))
      (set-process-filter proc 'darcsum-process-filter))
    (with-current-buffer buf
      (set (make-variable-buffer-local 'darcsum-logfile) tempfile)
      (set (make-variable-buffer-local 'darcsum-changeset-to-record) changeset)
      (set (make-variable-buffer-local 'darcsum-parent-buffer) parent-buf))))

(defun darcsum-record ()
  (interactive)
  (window-configuration-to-register darcsum-register)
  (let ((parent-buf (current-buffer))
	(buf (get-buffer-create "*darcs comment*"))
	(changeset (darcsum-selected-changeset t)))
    (switch-to-buffer-other-window buf)
    (darcsum-comment-mode)
    (set (make-variable-buffer-local 'darcsum-changeset-to-record) changeset)
    (set (make-variable-buffer-local 'darcsum-parent-buffer) parent-buf)
    (message
"Title of change on first line, long comment after.  C-c C-c to record.")))

(defun darcsum-send (recipient)
  (interactive "sSend changes to: ")
  (let ((parent-buf (current-buffer))
	(changeset (darcsum-selected-changeset t))
	(buf (generate-new-buffer " *darcs send*")))
    (message "Sending changes...")
    (let ((proc (start-process "darcs" buf
			       darcsum-program "send")))
      (set-process-filter proc 'darcsum-process-filter))
    (with-current-buffer buf
      (set (make-variable-buffer-local 'darcsum-changeset-to-record) changeset)
      (set (make-variable-buffer-local 'darcsum-parent-buffer) parent-buf)
      (set (make-variable-buffer-local 'darcsum-process-arg) recipient))))

(defun darcsum-revert ()
  (interactive)
  (when (yes-or-no-p "Really revert these changes? ")
    (let ((parent-buf (current-buffer))
	  (changeset (darcsum-selected-changeset t))
	  (buf (generate-new-buffer " *darcs revert*")))
      (message "Reverting changes...")
      (let ((proc (start-process "darcs" buf
				 darcsum-program "revert")))
	(set-process-filter proc 'darcsum-process-filter))
      (with-current-buffer buf
	(set (make-variable-buffer-local 'darcsum-changeset-to-record) changeset)
	(set (make-variable-buffer-local 'darcsum-parent-buffer) parent-buf)))))

(defvar darcsum-comment-mode-map ())
(if darcsum-comment-mode-map
    ()
  (setq darcsum-comment-mode-map (make-keymap))
  (define-key darcsum-comment-mode-map "\C-x\C-s" 'darcsum-really-record)
  (define-key darcsum-comment-mode-map "\C-c\C-c" 'darcsum-really-record))

(defun darcsum-comment-mode ()
  "Major mode for output from \\[darcsum-comment]."
  (interactive)
  (kill-all-local-variables)
  (indented-text-mode)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (use-local-map darcsum-comment-mode-map)
  (setq major-mode 'darcsum-comment-mode
	mode-name "Darcs Summary")
  (run-hooks 'darcsum-comment-mode-hook))

;;; Major Mode

(defun darcsum-reposition ()
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (goto-char (+ (line-beginning-position) 13)))
     ((eq type 'file)
      (goto-char (+ (line-beginning-position) 38)))
     ((eq type 'change)
      (goto-char (line-beginning-position))))))

(defsubst darcsum-other-buffer (other-buffer)
  (let ((buf (or other-buffer (generate-new-buffer "*darcs*"))))
    (with-current-buffer buf
      (unless (eq major-mode 'darcsum-mode)
	(darcsum-mode))
      (current-buffer))))

(defun darcsum-move (other-buffer)
  (interactive "BMove change to (RET creates new patch): ")
  (let ((buf (darcsum-other-buffer other-buffer))
	(changeset (darcsum-selected-changeset)))
    (setq darcsum-data
	  (darcsum-remove-changeset darcsum-data changeset))
    (with-current-buffer buf
      (let ((inhibit-redisplay t))
	(darcsum-apply-to-changeset
	 changeset
	 (function
	  (lambda (dir file change)
	    (cond
	     ((listp (car change))
	      (setcar change (caar change)))
	     ((and (numberp (car change))
		   (< (car change) 0))
	      (setcar change (abs (car change))))))))
	(setq darcsum-data
	      (darcsum-add-changeset darcsum-data changeset))
	(darcsum-refresh))))
  (darcsum-refresh))

(defun darcsum-find-file ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (find-file (get-text-property (point) 'darcsum-dir)))
     ((eq type 'file)
      (find-file (expand-file-name
		  (get-text-property (point) 'darcsum-file)
		  (get-text-property (point) 'darcsum-dir))))
     ((eq type 'change)
      (let ((change-line (car (get-text-property (point) 'darcsum-change))))
	(find-file (expand-file-name
		    (get-text-property (point) 'darcsum-file)
		    (get-text-property (point) 'darcsum-dir))))))))

(defun darcsum-goto ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (find-file-other-window
       (get-text-property (point) 'darcsum-dir)))
     ((eq type 'file)
      (find-file-other-window
       (expand-file-name
	(get-text-property (point) 'darcsum-file)
	(get-text-property (point) 'darcsum-dir))))
     ((eq type 'change)
      (let ((change-line (car (get-text-property (point) 'darcsum-change))))
	(find-file-other-window
	 (expand-file-name
	  (get-text-property (point) 'darcsum-file)
	  (get-text-property (point) 'darcsum-dir)))
	(if (listp change-line)
	    (setq change-line (car change-line)))
	(goto-line (abs change-line)))))))

(defun darcsum-toggle-mark ()
  (interactive)
  (let ((changeset (darcsum-changeset-at-point t)))
    (darcsum-apply-to-changeset changeset
				(function
				 (lambda (dir file change)
				   (if (listp (car change))
				       (setcar change (caar change))
				     (setcar change (list (car change))))))))
  (darcsum-refresh))

(defun darcsum-toggle ()
  (interactive)
  (let* ((changeset (darcsum-changeset-at-point t))
	 (any-visible
	  (darcsum-applicable-p
	   changeset (function
		      (lambda (d f change)
			(let ((item (darcsum-change-item change)))
			  (and (numberp item) (> item 0))))))))
    (darcsum-apply-to-changeset
     changeset (function
		(lambda (dir file change)
		  (let ((item (darcsum-change-item change)))
		    (if (numberp item)
			(if any-visible
			    (setcar change (- (abs item)))
			  (if (listp (car change))
			      (setcar change (list (abs item)))
			    (setcar change (abs item))))))))))
  (darcsum-refresh))

(defun darcsum-refresh ()
  (interactive)
  (let ((line (count-lines (point-min) (point)))
	(inhibit-redisplay t))
    (if (/= (point) (line-beginning-position))
	(setq line (1- line)))
    (darcsum-display-changeset darcsum-data)
    (goto-char (point-min))
    (forward-line line)
    (darcsum-reposition)))

(defun darcsum-next-entity (&optional arg backward)
  (interactive "p")
  (funcall (if backward 'previous-line 'next-line) arg)
  (goto-char (line-beginning-position))
  (while (and (not (if backward (bobp) (eobp)))
	      (looking-at "^[+-]"))
    (forward-line (if backward -1 1)))
  (unless (get-text-property (point) 'darcsum-line-type)
    (goto-char (if backward (point-max) (point-min)))
    (forward-line (if backward -3 3)))
  (darcsum-reposition))

(defun darcsum-next-line (&optional arg)
  (interactive "p")
  (darcsum-next-entity arg))

(defun darcsum-previous-line (&optional arg safety)
  (interactive "p")
  (darcsum-next-entity arg t))

(defsubst darcsum-original-path (pos)
  (let ((path (expand-file-name
	       (get-text-property pos 'darcsum-file)
	       (expand-file-name
		(get-text-property pos 'darcsum-dir)
		"_darcs/current"))))
    (if (file-readable-p path)
	path)))

(defsubst darcsum-path (pos)
  (expand-file-name (get-text-property pos 'darcsum-file)
		    (get-text-property pos 'darcsum-dir)))

(defun darcsum-diff ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
	  (eq type 'change))
      (diff (darcsum-original-path (point))
	    (darcsum-path (point)))))))

(defun darcsum-delete ()
  (interactive)
  (setq darcsum-data
	(darcsum-remove-changeset darcsum-data
				  (darcsum-selected-changeset)))
  (darcsum-refresh))

(defun darcsum-remove ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir)
      (error "Cannot remove whole directories yet, try file by file for now"))
     ((memq type '(file change))
      (let* ((dir (get-text-property (point) 'darcsum-dir))
	     (dentry (and dir (assoc dir darcsum-data)))
	     (file (get-text-property (point) 'darcsum-file))
	     (fentry (assoc file dentry))
	     (sym (darcsum-change-item (cadr fentry)))
	     file-to-remove)
	(cond
	 ((not (symbolp sym))
	  (when (yes-or-no-p
		 (format "Really delete file with changes '%s'? " file))
	    (delete-file (expand-file-name file dir))
	    (setq file-to-remove file)))
	 ((eq sym 'newfile)
	  (delete-file (expand-file-name file dir)))
	 ((eq sym 'addfile)
	  (setq file-to-remove file)
	  (delete-file (expand-file-name file dir)))
	 (t
	  (error "Removing makes no sense for that entry")))
	(if file-to-remove
	    (with-temp-buffer
	      (cd (expand-file-name dir))
	      (if (/= 0 (call-process darcsum-program nil t nil
				      "remove" file-to-remove))
		  (error "Error running darcsum remove"))))))))
  (darcsum-redo))

(defun darcsum-add ()
  (interactive)
  (dolist (dir (darcsum-selected-changeset))
    (dolist (file (cdr dir))
      (let ((item (darcsum-change-item (cadr file))))
	(if (and (symbolp item) (eq item 'newfile))
	    (progn
	      (setcar (cadr file) 'addfile)
	      (with-temp-buffer
		(cd (expand-file-name (car dir)))
		(if (/= 0 (call-process darcsum-program nil t nil
					"add" (car file)))
		    (error "Error running darcsum add for '%s' in dir '%s'"
			   (car file) (car dir)))))
	  (error "Can only add New entries for '%s' in dir '%s'"
		 (car file) (car dir))))))
  (darcsum-refresh))

(defun darcsum-add-to-boring (path)
  (interactive
   (let ((type (get-text-property (point) 'darcsum-line-type)))
     (cond
      ((eq type 'dir)
       (setq path (get-text-property (point) 'darcsum-dir))
       (if (string-match "^\\./" path)
	   (setq path (substring path 2)))
       (setq path (concat "(^|/)" (regexp-quote path) "($|/)")))
      ((memq type '(file change))
       (setq path (get-text-property (point) 'darcsum-file))
       (setq path (concat "(^|/)" path "$"))))
     (list (read-string "Add to boring list: " path))))
  (save-excursion
    (set-buffer (find-file-noselect "_darcs/prefs/boring"))
    (goto-char (point-max))
    (insert path ?\n)
    (save-buffer)
    (kill-buffer (current-buffer)))
  (darcsum-redo))

(defun darcsum-add-change-log-entry ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
	  (eq type 'change))
      (darcsum-goto)
      (add-change-log-entry)))))

(defun darcsum-ediff ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
	  (eq type 'change))
      (ediff (darcsum-original-path (point))
	     (darcsum-path (point)))))))

(defun darcsum-ediff-merge ()
  (interactive)
  (let ((type (get-text-property (point) 'darcsum-line-type)))
    (cond
     ((eq type 'dir))
     ((or (eq type 'file)
	  (eq type 'change))
      (ediff-merge (darcsum-original-path (point))
		   (darcsum-path (point)))))))

(defun darcsum-redo (&optional arg)
  (interactive "P")
  (let ((dir default-directory)
	(look-for-adds (or arg darcsum-look-for-adds))
	(darcsum-default-expanded t))
    (message "Re-running darcsum-whatsnew")
    (let ((changes (darcsum-whatsnew dir look-for-adds t)))
      (setq darcsum-data
	    (darcsum-merge-changeset darcsum-data changes)))
    (darcsum-refresh)))

(defun darcsum-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defvar darcsum-mode-abbrev-table nil
  "Abbrev table used while in darcsum-mode mode.")
(define-abbrev-table 'darcsum-mode-abbrev-table ())

(defvar darcsum-mode-map nil)
(if darcsum-mode-map
    ()		   ; Do not change the keymap if it is already set up.
  (setq darcsum-mode-map (make-keymap))
  (suppress-keymap darcsum-mode-map)
  (define-key darcsum-mode-map [return] 'darcsum-toggle)
  (define-key darcsum-mode-map "\C-m" 'darcsum-toggle)
  (define-key darcsum-mode-map "\C-c\C-c" 'darcsum-goto)
  (define-key darcsum-mode-map "f" 'darcsum-find-file)
  (define-key darcsum-mode-map "=" 'darcsum-diff)
  (define-key darcsum-mode-map "e" 'darcsum-ediff)
  (define-key darcsum-mode-map "E" 'darcsum-ediff-merge)
  (define-key darcsum-mode-map "g" 'darcsum-redo)
  (define-key darcsum-mode-map "n" 'darcsum-next-line)
  (define-key darcsum-mode-map "p" 'darcsum-previous-line)
  (define-key darcsum-mode-map "a" 'darcsum-add)
  (define-key darcsum-mode-map "l" 'darcsum-add-change-log-entry)
  (define-key darcsum-mode-map "c" 'darcsum-record)
  (define-key darcsum-mode-map "R" 'darcsum-record)
  (define-key darcsum-mode-map "U" 'darcsum-revert)
  (define-key darcsum-mode-map "d" 'darcsum-delete)
  (define-key darcsum-mode-map "r" 'darcsum-remove)
  (define-key darcsum-mode-map "M" 'darcsum-move)
  (define-key darcsum-mode-map "m" 'darcsum-toggle-mark)
  (define-key darcsum-mode-map "i" 'darcsum-add-to-boring)
  (define-key darcsum-mode-map "B" 'darcsum-add-to-boring)
  (define-key darcsum-mode-map "q" 'darcsum-quit))

(define-derived-mode darcsum-mode text-mode "Darcs"
  "Darcs summary mode is for previewing changes to become part of a patch.
\\{darcsum-mode-map}"
  (font-lock-mode -1)
  (use-local-map darcsum-mode-map))

;;; This is the entry code, M-x darcsum (or M-x darcs-summary)

(defun darcsum-display (data &optional look-for-adds)
  (with-current-buffer (generate-new-buffer "*darcs*")
    (darcsum-mode)
    (set (make-variable-buffer-local 'darcsum-data) data)
    (set (make-variable-buffer-local 'darcsum-look-for-adds) look-for-adds)
    (darcsum-refresh)
    (goto-char (point-min))
    (forward-line 3)
    (darcsum-reposition)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun darcsum-whatsnew (directory &optional arg no-display)
  (interactive "DDirectory: \nP")
  (with-temp-buffer
    (cd directory)
    (unless (file-directory-p (expand-file-name "_darcs" directory))
      (error "Directory '%s' is not under darcs version control"
	     directory))
    (let ((result
	   (if (null arg)
	       (call-process darcsum-program nil t nil
			     "whatsnew" "--no-summary")
	     (call-process darcsum-program nil t nil
			   "whatsnew" "--no-summary" "--look-for-adds"))))
      (if (/= result 0)
	  (error "Error running darcsum whatsnew")
	(let ((changes (darcsum-read-changeset darcsum-default-expanded)))
	  (if (and changes (not no-display))
	      (darcsum-display changes arg))
	  changes)))))

;;;###autoload
(defun darcsum-view (directory)
  (interactive "DApply to directory: ")
  (unless (file-directory-p (expand-file-name "_darcs" directory))
    (error "Directory '%s' is not under darcs version control"
	   directory))
  (if (or (and (search-forward "{" nil t)
	       (goto-char (1- (point))))
	  (search-backward "{" nil t))
      (let ((changes (darcsum-parse-changeset))
	    (default-directory directory))
	(darcsum-display changes))
    (error "Cannot find a darcs patch in the current buffer")))

;;; Gnus integration code, for viewing darcs patches in a changeset
;;; buffer.  They cannot be recorded from there, however, since the
;;; changes have not been applied to the working tree.  To do this,
;;; you must still pipe the message to "darcs apply".  This code only
;;; works as a browser for now.

(defvar darcsum-install-gnus-code nil)

(when darcsum-install-gnus-code
  (defun mm-view-darcs-patch (handle)
    "View HANDLE as a darcs patch, using darcsum.el."
    (let* ((name (mail-content-type-get (mm-handle-type handle) 'name))
	   (directory
	    (read-directory-name "Apply patch to directory: ")))
      (mm-with-unibyte-buffer
	(mm-insert-part handle)
	(let ((coding-system-for-write 'binary))
	  (goto-char (point-min))
	  (darcsum-view directory)
	  (delete-other-windows)))))

  (defun gnus-mime-view-darcs-patch ()
    "Pipe the MIME part under point to a process."
    (interactive)
    (gnus-article-check-buffer)
    (let ((data (get-text-property (point) 'gnus-data)))
      (when data
	(mm-view-darcs-patch data))))

  (defun gnus-article-view-darcs-patch (n)
    "Pipe MIME part N, which is the numerical prefix."
    (interactive "p")
    (gnus-article-part-wrapper n 'mm-view-darcs-patch))

  (eval-after-load "gnus-art"
    '(progn
       (nconc gnus-mime-action-alist
	      '(("apply darcs patch" . gnus-mime-view-darcs-patch)))
       (nconc gnus-mime-button-commands
	      '((gnus-mime-view-darcs-patch "V" "Apply darcs patch...")))))

  (defun gnus-summary-view-darcs-patch (directory)
    "Apply the current article as a darcs patch to DIRECTORY."
    (interactive "DApply patch to directory: ")
    (gnus-summary-select-article)
    (let ((mail-header-separator ""))
      (gnus-eval-in-buffer-window gnus-article-buffer
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (darcsum-view directory)))))

  (eval-after-load "gnus-sum"
    '(progn
       (define-key gnus-summary-mime-map "V" 'gnus-article-view-darcs-patch)
       (define-key gnus-summary-article-map "V" 'gnus-summary-view-darcs-patch))))

;;; darcsum.el ends here
