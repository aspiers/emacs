;;; xtla-xemacs.el --- Compatibility stuff for XEmacs

;; Copyright (C) 2004 by Stefan Reichoer

;; Author: Robert Widhopf-Fenk <hack@robf.de>

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

;; Unfortunately GNU and XEmacs are only 98% compatible on the level of lisp
;; code.  This file provides functions currently missing in XEmacs.  You
;; should not rely on anything within this file, since functions, etc. will
;; be removed when they become available in XEmacs or conflict with other
;; packages.  Instead of hacking other xtla files with (if (featurep 'xemacs)
;; ...) we should rely on compatible lisp.


;;; Code:
(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-end-position ()
    (save-excursion (end-of-line) (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun line-beginning-position (&optional n)
  (save-excursion
    (if n (forward-line n))
    (beginning-of-line)
    (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'match-string-no-properties)
  (defun match-string-no-properties (arg)
    (match-string arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; his is an ugly hack to a bug in diff-mode.el that causes fontification to
;; fail with an error.  I filed a patch to xemacs-patches and hopefully this
;; code can be removed after new fixed packages become avaialable.
(require 'diff-mode)
(when (not (boundp 'diff-font-lock-keywords-1))
  (defvar diff-font-lock-keywords-1 diff-font-lock-keywords)
  (setq diff-font-lock-keywords '(diff-font-lock-keywords-1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clone-buffer (&optional newname display-flag)
  "Create a twin copy of the current buffer.
If NEWNAME is nil, it defaults to the current buffer's name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.

If DISPLAY-FLAG is non-nil, the new buffer is shown with `pop-to-buffer'.
This runs the normal hook `clone-buffer-hook' in the new buffer
after it has been set up properly in other respects."
  (interactive (list (if current-prefix-arg (read-string "Name: "))
		     t))
  (if buffer-file-name
      (error "Cannot clone a file-visiting buffer"))
  (if (get major-mode 'no-clone)
      (error "Cannot clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let ((buf (current-buffer))
	(ptmin (point-min))
	(ptmax (point-max))
	(pt (point))
	(mk (mark t))
	(modified (buffer-modified-p))
	(mode major-mode)
	(lvars (buffer-local-variables))
	(process (get-buffer-process (current-buffer)))
	(new (generate-new-buffer (or newname (buffer-name)))))
    (save-restriction
      (widen)
      (with-current-buffer new
	(insert-buffer-substring buf)))
    (with-current-buffer new
      (narrow-to-region ptmin ptmax)
      (goto-char pt)
      (if mk (set-mark mk))
      (set-buffer-modified-p modified)

      ;; Clone the old buffer's process, if any.
      (when process (clone-process process))

      ;; Now set up the major mode.
      (funcall mode)

      ;; Set up other local variables.
      (mapcar (lambda (v)
		(condition-case ()	;in case var is read-only
		    (if (symbolp v)
			(makunbound v)
		      (set (make-local-variable (car v)) (cdr v)))
		  (error nil)))
	      lvars)

      ;; Run any hooks (typically set up by the major mode
      ;; for cloning to work properly).
      (run-hooks 'clone-buffer-hook))
    (if display-flag (pop-to-buffer new))
    new))

(defun clone-process (process &optional newname)
  "Create a twin copy of PROCESS.
If NEWNAME is nil, it defaults to PROCESS' name;
NEWNAME is modified by adding or incrementing <N> at the end as necessary.
If PROCESS is associated with a buffer, the new process will be associated
  with the current buffer instead.
Returns nil if PROCESS has already terminated."
  (setq newname (or newname (process-name process)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (when (memq (process-status process) '(run stop open))
    (let* ((process-connection-type (process-tty-name process))
	   (old-kwoq (process-kill-without-query process nil))
	   (new-process
	    (if (memq (process-status process) '(open))
		(apply 'open-network-stream newname
		       (if (process-buffer process) (current-buffer)))
	      (apply 'start-process newname
		     (if (process-buffer process) (current-buffer))
		     (process-command process)))))
      (process-kill-without-query new-process old-kwoq)
      (process-kill-without-query process old-kwoq)
      (set-process-filter new-process (process-filter process))
      (set-process-sentinel new-process (process-sentinel process))
      new-process)))

(provide 'xtla-xemacs)

;; Local Variables:
;; arch-tag: 8fb45acb-2711-4d20-ad36-1175f2634578
;; End:
