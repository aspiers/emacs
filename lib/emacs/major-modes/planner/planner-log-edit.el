;;; planner-log-edit.el --- Record VC commits as a note in todays planner file

;; Copyright (C) 2004 Simon Winwood (sjw AT cse.unsw.edu.au)
;; Parts copyright (C) 2004 Dryice Dong Liu (dryice AT liu.com.cn)

;; Author: Simon Winwood <sjw@cse.unsw.edu.au>
;; Version: 0.1
;; Keywords: planner, vc, pcl-cvs, log-edit

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

;; This file records cvs (and VC) commits into todays planner file.

;;; Code:

(require 'planner)
(require 'log-edit)

(defgroup planner-log-edit nil
  "Planner options for log-edit."
  :prefix "planner-log-edit-"
  :group 'planner)

(defcustom planner-log-edit-include-files-flag
  t
  "Non-nil means include a list of committed files in the note."
  :type 'boolean
  :group 'planner-log-edit)

(defcustom planner-log-edit-quote-filenames-flag
  nil
  "Non-nil means quote the file names with \"=\"."
  :type 'boolean
  :group 'planner-log-edit)

(defcustom planner-log-edit-notice-commit-function t
  "Function that should return non-nil if this commit should be noted.
The function will be run in the log buffer."
  :type '(choice
          (const :tag "Always note commits" t)
          function)
  :group 'planner-log-edit)

(defcustom planner-log-edit-flush-regexp-list
  nil
  "List of regexps to flush before inserting note"
  :type '(repeat regexp)
  :group 'planner-log-edit)

(defcustom planner-log-edit-module-name-function 'planner-log-edit-cvs-module
  "Function that should return a name for the current module."
  :type 'function
  :group 'planner-log-edit)

(defun planner-log-edit-quote-file-maybe (arg)
  "Quote ARG if `planner-log-edit-quote-filenames-flag is non-nil."
  (if planner-log-edit-quote-filenames-flag
      (concat "=" arg "=")
    arg))

(defun planner-log-edit-cvs-module ()
  (condition-case nil
      (when (fboundp 'cvs-get-module) (cvs-get-module))))

;;;###autoload
(defun planner-log-edit-add-note ()
  "Add a note describing the commit to the current planner page."
  (let* ((buffer (current-buffer))
         (files (log-edit-files))
         ;; This should be a function call into log-edit, but until it
         ;; exists ...
         (module-name (funcall planner-log-edit-module-name-function)))
    (if (if (functionp planner-log-edit-notice-commit-function)
            (funcall planner-log-edit-notice-commit-function)
          planner-log-edit-notice-commit-function)
        (save-excursion
          (save-window-excursion
            (planner-create-note nil)
            (insert "Commit"
                    (if module-name
                        (concat " in "
                                (planner-log-edit-quote-file-maybe
                                 module-name))
                      ""))
            (newline)
            (when planner-log-edit-include-files-flag
              (insert "Files: ")
              (insert (mapconcat 'planner-log-edit-quote-file-maybe files " "))
              (newline)
              (newline))
            (insert
             (with-temp-buffer
               (insert-buffer-substring buffer)
               (goto-char (point-min))
               (mapc (lambda (regexp)
                       (flush-lines regexp))
                     planner-log-edit-flush-regexp-list)
               (buffer-string)))
            ;(insert-buffer-substring buffer)
            )))))

(add-hook 'log-edit-done-hook 'planner-log-edit-add-note)

(provide 'planner-log-edit)

;;; planner-log-edit.el ends here
