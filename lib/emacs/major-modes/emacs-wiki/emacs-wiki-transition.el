;;; emacs-wiki-transition.el --- Help users adjust to certain changes.

;; Copyright (C) 2004 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-menu.el
;; Keywords: hypermedia transition
;; Author: Michael Olson (mwolson AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Help emacs-wiki users adjust to certain changes.
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: Emacs20, Emacs21, XEmacs21

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;_* Commentary

;; This file is meant to help out users who need to adjust to changes
;; in the Wiki markup or recover from certain manglings of old
;; emacs-wiki versions.

;;;_ + Startup

;; This is not meant to be included in your .emacs file.  Please avoid
;; doing so.

;;;_ + Usage

;; You should manually browse this file for functions that you might
;; need.  Descriptions and usage scenarios will be included in each
;; function definition.
;;
;; To execute one of these functions, do a
;;
;; (load "path/to/emacs-wiki/emacs-wiki-transition.el")
;;
;; and then type M-x <name-of-funtion>, where <name-of-function>
;; represents the function that you want to execute.
;;
;; You will be prompted when you execute one of these functions just
;; to make sure that nothing bad happens by accident.  To disable the
;; prompt, append `t' to the list of parameters or hit C-u before
;; running the function.

;;;_ + Contributors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Transitional Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki)

;;; Demangle spaces in the second part of extended links

(defun emacs-wiki-tr-extended-link-spaces-this-buffer
  (&optional no-prompt)
  "Replace instances of \"%20\" with a space in the current buffer.
You will want to use this so that the link descriptions can look
right.

This helps to fix a bug in emacs-wiki versions before \"2.63\".

If NO-PROMPT is non-nil, skip the confirmation prompt.

You should only need to do this once."
  (interactive)
  (when (or no-prompt
            (yes-or-no-p
             (concat "Shall we demangle spaces in extended links "
                     "for the current buffer? ")))
    (save-excursion
      (save-match-data
        (let (matched-text)
          (goto-char (point-min))
          (while (re-search-forward
                  emacs-wiki-extended-link-regexp nil t)
            (setq matched-text (emacs-wiki-match-string-no-properties 3))
            ;; replace 3rd match (description part of extended link)
            (when matched-text
              (replace-match (replace-regexp-in-string
                              "%20" " " matched-text)
                             nil nil nil 3))))))))

(defun emacs-wiki-tr-extended-link-spaces-one-file
  (file &optional no-prompt)
  "Replace instances of \"%20\" with a space in FILE.
You will want to use this so that the link descriptions can look
right.

This helps to fix a bug in emacs-wiki versions before \"2.63\".

If NO-PROMPT is non-nil, skip the confirmation prompt.

You should only need to do this once."
  (interactive)
  (when (or no-prompt
            (yes-or-no-p
             (concat "Shall we demangle spaces in extended links "
                     "for the " file " file? ")))
    (save-excursion
      ;; only kill the buffer if it was not already open
      (let ((buffer-opened-already
             (get-file-buffer file))
            (buffer-to-kill
             (set-buffer (find-file-noselect file))))
        (emacs-wiki-tr-extended-link-spaces-this-buffer t)
        (save-buffer t)
        (unless buffer-opened-already
          (kill-buffer buffer-to-kill))))))

(defun emacs-wiki-tr-extended-link-spaces-one-project
  (project &optional no-prompt)
  "Replace instances of \"%20\" with a space in PROJECT.
You will want to use this so that the link descriptions can look
right.

This helps to fix a bug in emacs-wiki versions before \"2.63\".

If NO-PROMPT is non-nil, skip the confirmation prompt.

You should only need to do this once."
  (interactive)
  (when (or no-prompt
            (yes-or-no-p
             (concat "Shall we demangle spaces in extended links "
                     "for the " project " project? ")))
    ;; make sure file alist has been generated by now
    (emacs-wiki-file-alist)
    (dolist (file (cadr (assoc project emacs-wiki-file-alist)))
      (emacs-wiki-tr-extended-link-spaces-one-file (cdr file) t))
    (message (concat "Finished processing " project))))

(defun emacs-wiki-tr-extended-link-spaces-all-projects
  (&optional no-prompt)
  "Replace instances of \"%20\" with a space in all projects.
You will want to use this so that the link descriptions can look
right.

This helps to fix a bug in emacs-wiki versions before \"2.63\".

If NO-PROMPT is non-nil, skip the confirmation prompt.

You should only need to do this once."
  (interactive)
  (when (or no-prompt
            (yes-or-no-p
             (concat "Shall we demangle spaces in extended links "
                     "for all projects? ")))
    (dolist (project emacs-wiki-projects)
      (emacs-wiki-tr-extended-link-spaces-one-project (car project) t))
    (message "All projects have been processed")))

;;; emacs-wiki-transition.el ends here
