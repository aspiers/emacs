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



;;; History:
;;
;; This started with a few functions protected by (fboundp ) in
;; xtla.el, and was splitted afterwards.

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'xtla-defs)
  (require 'overlay))

(require 'wid-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixes warnings about undefined variables
(unless (boundp 'add-log-buffer-file-name-function)
  (defvar add-log-buffer-file-name-function nil))
(unless (boundp 'add-log-file-name-function)
  (defvar add-log-file-name-function nil))
(unless (boundp 'add-log-keep-changes-together)
  (defvar add-log-keep-changes-together nil))
(unless (boundp 'global-font-lock-mode)
  (defvar global-font-lock-mode nil))
(unless (boundp 'vc-ignore-vc-files)
  (defvar vc-ignore-vc-files nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'add-log-file-name)
  (defun add-log-file-name (buffer-file log-file)
    ;; Never want to add a change log entry for the ChangeLog file itself.
    (unless (or (null buffer-file) (string= buffer-file log-file))
      (if add-log-file-name-function
          (funcall add-log-file-name-function buffer-file)
        (setq buffer-file
              (if (string-match
                   (concat "^" (regexp-quote (file-name-directory log-file)))
                   buffer-file)
                  (substring buffer-file (match-end 0))
                (file-name-nondirectory buffer-file)))
        ;; If we have a backup file, it's presumably because we're
        ;; comparing old and new versions (e.g. for deleted
        ;; functions) and we'll want to use the original name.
        (if (backup-file-name-p buffer-file)
            (file-name-sans-versions buffer-file)
          buffer-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string
                                          &optional fixedcase literal)
    (replace-in-string string regexp rep literal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'line-end-position)
  (defun line-end-position ()
    (save-excursion (end-of-line) (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'line-beginning-position)
  (defun line-beginning-position (&optional n)
    (save-excursion
      (if n (forward-line n))
      (beginning-of-line)
      (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'mouse-set-point)
  (defun mouse-set-point (event)
    (goto-char (event-point event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'match-string-no-properties)
  (defun match-string-no-properties (arg &optional string)
    (match-string arg string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'clone-buffer)
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
                  (condition-case ()    ;in case var is read-only
                      (if (symbolp v)
                          (makunbound v)
                        (set (make-local-variable (car v)) (cdr v)))
                    (error nil)))
                lvars)

        ;; Run any hooks (typically set up by the major mode
        ;; for cloning to work properly).
        (run-hooks 'clone-buffer-hook))
      (if display-flag (pop-to-buffer new))
      new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AFAIK easy-menu cannot be used for dynamic menus

(defun tla-xemacs-tla-mode-p (buf)
  "Helper function for menu-related functions.

Return t if BUF is a tla-related buffer."
  (if (bufferp buf)
      (setq buf (format "%s" (symbol-value-in-buffer 'major-mode buf))))
  (string-match "^tla-" buf))

(defvar tla--dead-process-buffer-queue nil)

(defun tla-xemacs-buffers-menu (menu)
  "Create the markers-menu.

MENU is the menu to which items should be added."
  (interactive (list nil))
  (let ((bufs (buffer-list))
        (queue tla--dead-process-buffer-queue)
        queue-menu
        b)
    ;; the user buffers
    (while bufs
      (setq b (car bufs)
            bufs (cdr bufs))
      (if (tla-xemacs-tla-mode-p b)
          (setq menu (cons (vector (buffer-name b)
                                   (list 'switch-to-buffer b) t)
                           menu))))
    (setq menu (sort menu
                     (lambda (m1 m2) (string< (aref m1 0) (aref m2 0)))))
    ;; the queue buffers
    (while queue
      (setq b (car queue)
            queue (cdr queue)
            queue-menu (cons (vector (buffer-name b)
                                     (list 'switch-to-buffer b) t)
                             queue-menu)))
    (setq queue-menu (sort queue-menu
                           (lambda (m1 m2) (string< (aref m1 0) (aref m2 0)))))
    ;; combine menus
    (setq menu (cons (append '("Queue") queue-menu) menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tla-group-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-grouping-function'.
This groups buffers by major mode.  It only really makes sense if
`buffers-menu-sorting-function' is
'tla-sort-buffers-menu-by-mode-then-alphabetically'.

 (setq buffers-menu-grouping-function 'tla-group-buffers-menu-by-mode-then-alphabetically)
BUF1 and BUF2 are successive members of the sorted buffers list after
being passed through `buffers-menu-sort-function'. It should return
non-nil if the second buffer begins a new group.

This is a modified version of
`group-buffers-menu-by-mode-then-alphabetically'
adding an submenu \"TLA\" containing all tla buffers."
  (cond ((and buf1 buf2
              (not (tla-xemacs-tla-mode-p buf1))
              (tla-xemacs-tla-mode-p buf2))
         (if (string-match "\\`*" (buffer-name buf1))
             "*Misc*"
           (symbol-value-in-buffer 'mode-name buf1)))
        ((and buf1
              (tla-xemacs-tla-mode-p buf1)
              (or (not buf2)
                  (not (tla-xemacs-tla-mode-p buf2))))
         "TLA")
	((string-match "\\`*" (buffer-name buf1))
	 (and (null buf2) "*Misc*"))
        ((or (null buf2)
	     (string-match "\\`*" (buffer-name buf2))
	     (not (eq (symbol-value-in-buffer 'major-mode buf1)
		      (symbol-value-in-buffer 'major-mode buf2))))
	 (symbol-value-in-buffer 'mode-name buf1))
	(t nil)))

(defun tla-sort-buffers-menu-by-mode-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'.
Sorts first by major mode and then alphabetically by name, but puts buffers
beginning with a star at the end of the list.

 (setq buffers-menu-sort-function 'tla-sort-buffers-menu-by-mode-then-alphabetically)
It will be passed two arguments BUF1 and BUF2 (two buffers to compare)
and will return t if the first is \"less\" than the second.

This is a modified version of `sort-buffers-menu-by-mode-then-alphabetically',
causing all *tla-* buffers to be treated as having the same major mode."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (inv1p (not (null (string-match "\\` " nam1))))
	 (inv2p (not (null (string-match "\\` " nam2))))
	 (star1p (not (null (string-match "\\`*" nam1))))
	 (star2p (not (null (string-match "\\`*" nam2))))
	 (mode1 (symbol-value-in-buffer 'major-mode buf1))
	 (mode2 (symbol-value-in-buffer 'major-mode buf2)))
    (if (tla-xemacs-tla-mode-p mode1)
        (setq mode1 "tla"))
    (if (tla-xemacs-tla-mode-p mode1)
        (setq mode2 "tla"))
    (cond ((not (eq inv1p inv2p))
	   (not inv1p))
	  ((not (eq star1p star2p))
	   (not star1p))
	  ((and star1p star2p (string-lessp nam1 nam2)))
	  ((string-lessp mode1 mode2)
	   t)
	  ((string-lessp mode2 mode1)
	   nil)
	  (t
	   (string-lessp nam1 nam2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; since the custom.el coming with XEmacs does not know about the :inherit
;; keyword of defface we are dealing with it for our faces ...
(let ((faces (face-list)) face inherit)
  (while faces
    (setq face (car faces)
          faces (cdr faces))
    (when (string-match "^tla-" (format "%s" face))
      (setq inherit (assoc :inherit (car (custom-face-get-spec face))))
      (if inherit
          (set-face-parent face (cadr inherit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (functionp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (buffer-substring)))

(unless (functionp 'minibufferp)
  (defun minibufferp ()
    "Return non-nil if within a minibuffer."
    (equal (selected-window)
           (active-minibuffer-window))))

(unless (functionp 'diff-hunk-next)
  (defalias 'diff-hunk-next 'diff-next-hunk))

(unless (functionp 'diff-hunk-prev)
  (defalias 'diff-hunk-prev 'diff-prev-hunk))

;; From Gnus.
(defun tla-xmas-move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end buffer))

(defun tla-xmas-kill-all-overlays ()
  "Delete all extents in the current buffer."
  (map-extents (lambda (extent ignore)
		 (delete-extent extent)
		 nil)))

(defun tla-xmas-add-text-properties (start end props &optional object)
  (add-text-properties start end props object)
  (put-text-property start end 'start-closed nil object))

(defun tla-xmas-put-text-property (start end prop value &optional object)
  (put-text-property start end prop value object)
  (put-text-property start end 'start-closed nil object))

(defun tla-xmas-assq-delete-all (key alist)
  (let ((elem nil))
    (while (setq elem (assq key alist))
      (setq alist (delq elem alist)))
    alist))

(defalias 'tla-make-overlay 'make-extent)
(defalias 'tla-delete-overlay 'delete-extent)
(defalias 'tla-overlay-put 'set-extent-property)
(defalias 'tla-move-overlay 'tla-xmas-move-overlay)
(defalias 'tla-overlay-buffer 'extent-object)
(defalias 'tla-overlay-start 'extent-start-position)
(defalias 'tla-overlay-end 'extent-end-position)
(defalias 'tla-kill-all-overlays 'tla-xmas-kill-all-overlays)
(defalias 'tla-extent-detached-p 'extent-detached-p)
(defalias 'tla-add-text-properties 'tla-xmas-add-text-properties)
(defalias 'tla-put-text-property 'tla-xmas-put-text-property)
(defalias 'tla-deactivate-mark 'ignore)
(defalias 'tla-window-edges 'window-pixel-edges)
(defalias 'tla-assq-delete-all 'tla-xmas-assq-delete-all)
(defconst tla-mouse-face-prop 'highlight)
;; end from Gnus

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'xtla-xemacs)

;; Local Variables:
;; End:
;; arch-tag: 8fb45acb-2711-4d20-ad36-1175f2634578

;;; xtla-xemacs.el ends here
