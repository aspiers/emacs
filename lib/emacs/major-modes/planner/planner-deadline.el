;;; planner-deadlines.el --- Deadlines for planner.el

;; Copyright (C) 2004 Sandra Jean Chua (Sacha) <sacha AT free.net.ph>
;; Parts copyright (C) 2004 Dryice Dong Liu <dryice AT liu.com.cn>
;; http://sacha.free.net.ph/notebook/emacs/dev/planner/planner-deadline.el

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
;;
;; With the default setup, make your tasks of the form
;;
;; #A0 _ Some task {{Deadline: 2004.09.12}}
;;
;; Note: There must be at least one space after the colon.
;;
;; Run M-x planner-deadline-update  to update the task descriptions.

(require 'planner)

;;; Code:

;;; USER VARIABLES -----------------------------------------------------------

(defgroup planner-deadline nil
  "Deadline reports for planner.el."
  :prefix "planner-deadline"
  :group 'planner)

(defcustom planner-deadline-change-hook nil
  "Functions to run after `planner-deadline-change'.
Point will be on the same line as the task."
  :type 'hook
  :group 'planner-deadline)

(defcustom planner-deadline-regexp "\\({{Deadline:\\s-+\\([0-9]+\\.[0-9]+\\.[0-9]+\\)[^}]*}}\\)"
  "Regular expression for deadline data.
The special deadline string should be regexp group 1. The
date (YYYY.MM.DD) should be regexp group 2."
  :type 'regexp
  :group 'planner-deadline)

(defun planner-deadline-get-deadline-from-string (string)
  "Return the deadline in STRING."
  (save-match-data
    (if (string-match planner-deadline-regexp string)
        (planner-match-string-no-properties 2 string)
      nil)))

(defun planner-deadline-get-current-deadline ()
  "Return the deadline of the current task."
  (planner-deadline-get-deadline-from-string
   (buffer-substring (planner-line-beginning-position)
                     (planner-line-end-position))))

(defun planner-deadline-days-left (deadline date)
  "Return how many days are left for DEADLINE with effective DATE."
  (let (diff
        (date (if (listp date) (planner-task-date date) date)))
    (if date
        (setq diff
              (- (calendar-absolute-from-gregorian
                  (planner-filename-to-calendar-date
                   deadline))
                 (calendar-absolute-from-gregorian
                  (planner-filename-to-calendar-date
                   date))))
      (setq date
            (if (string-match planner-date-regexp (planner-page-name))
                (planner-page-name)
              (planner-today))))
    (unless (string< deadline (planner-today))
      (setq diff
            (- (calendar-absolute-from-gregorian
                (planner-filename-to-calendar-date
                 deadline))
               (calendar-absolute-from-gregorian
                (planner-filename-to-calendar-date
                 date)))))
    diff))

(defun planner-deadline-calculate-string (deadline &optional date)
  "Return a deadline string for DEADLINE and effective DATE."
  (let ((diff (planner-deadline-days-left deadline date)))
    (concat "{{Deadline: "
            deadline
            " - "
            (cond
             ((< diff 0) (format "%d %s *OVERDUE*"
                                 (- diff)
                                 (if (= diff -1) "day"
                                   "days")))
             ((= diff 0) "*TODAY*")
             (t (format "%d %s" diff
                        (if (= diff 1)
                            "day"
                          "days"))))
            "}}")))

;;;###autoload
(defun planner-deadline-update ()
  "Replace the text for all tasks with deadlines.
By default, deadlines are of the form {{Deadline: yyyy.mm.dd}}.
See `planner-deadline-regexp' for details."
  (interactive)
  (with-planner-update-setup
   (goto-char (point-min))
   (while (re-search-forward planner-deadline-regexp nil t)
     (let* ((deadline (match-string 2))
            (task-info (save-match-data (planner-current-task-info)))
            (status (planner-task-status task-info))
            (end (match-end 0))
            new)
       (save-match-data
         (if task-info
             (unless (or (equal status "X") (equal status "C"))
               (setq new (planner-deadline-calculate-string deadline task-info)))
           (setq new (planner-deadline-calculate-string deadline nil))))
       (when new
         (if task-info
             (when (string-match planner-deadline-regexp (planner-task-description task-info))
               (planner-edit-task-description
                (replace-match new t t (planner-task-description task-info))))
           (replace-match new t t)
           (when (planner-current-note-info) (planner-update-note))))
       (goto-char (1+ end))))))

;;;###autoload
(defun planner-deadline-change (date)
  "Change the deadline of current task to DATE.
If DATE is nil, remove deadline."
  (interactive (list (planner-read-date)))
  (let* ((info (planner-current-task-info))
	 (description (planner-task-description info)))
    (when (string-match (concat "\\s-*" planner-deadline-regexp) description)
      (setq description (replace-match "" t t description)))
    (if date
	(planner-edit-task-description (concat description
					       " {{Deadline: "
					       date
					       "}}"))
      (planner-edit-task-description description)))
  (run-hooks 'planner-deadline-change-hook))

(provide 'planner-deadline)

;;; planner-deadline.el ends here
