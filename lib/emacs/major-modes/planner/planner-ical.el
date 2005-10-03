;;; planner-ical.el --- Import/Export planner tasks in the icalendar standard (RFC 2445)

;; Copyright (C) 2005 Chris Parsons

;; Emacs Lisp Archive Entry
;; Filename: planner-ical.el
;; Author: Chris Parsons <chris.p@rsons.org>
;; Maintainer: Chris Parsons <chris.p@rsons.org>
;; Keywords: planner, ical
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php

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

;; This module allows you to export planner tasks and notes in the
;; icalendar format (RFC 2445). For a description of the standard, see
;; http://www.ietf.org/rfc/rfc2445.txt.
;; 
;; In future, it is hoped that tasks will be importable also.
;; 
;; Note that this is very early days - currently we only export VTODO
;; items from just one page.
;;
;; Usage:
;;
;; Call planner-ical-export-page and specify the page, or
;; planner-ical-export-this-page. This produces a buffer. Save it
;; somewhere.

(require 'planner)
(require 'icalendar)

;;; Code:

;;;_+ User variables

(defgroup planner-ical nil
  "iCal (RFC 2445) support for planner.el."
  :prefix "planner-ical"
  :group 'planner)

(defcustom planner-ical-export-buffer "*Planner iCal Export*"
  "Buffer name for iCal exports from `planner-ical-export'."
  :type 'string
  :group 'planner-timeclock-summary)

(defun planner-ical-export-page (page &optional file)
  "Export PAGE's tasks in the Ical format.
If FILE is non-nil, results are saved to that file.
If FILE is nil, results are displayed in a `planner-ical-export-buffer'."
  (interactive (list (planner-read-name (planner-file-alist) "Enter the page to export: ")))
  (if file
      (with-temp-file file
        (planner-ical-export (if (listp page) page (list page))))
    (switch-to-buffer (get-buffer-create planner-ical-export-buffer))
    (erase-buffer)
    (planner-ical-export (if (listp page) page (list page)))))

(defun planner-ical-export (pages)
  "Export the given plan page to iCalendar format. The result
will be added to the current buffer."
  (let ((tasks (planner-extract-tasks pages))
        result)
    (while tasks
      (when (not (string= (planner-task-status (car tasks)) "X"))
        (let* ((task (car tasks))
               cat-list
               cat-list-no-dates
               (header (format "\nBEGIN:VTODO\nUID:emacs-planner-%x"
                               (sxhash (planner-task-description task))))
               (my-task-date (planner-task-date task))
               (task-date 
                (if my-task-date 
                    (planner-filename-to-calendar-date my-task-date)
                  nil))
               (task-categories
                (if (featurep 'planner-multi)
                    (progn (setq cat-list (planner-multi-task-link-as-list task))
                           (setq cat-list-no-dates nil)
                           (while cat-list
                             (let ((cat (car cat-list)))
                               (when (not (string-match planner-date-regexp cat)) 
                                 (setq cat-list-no-dates (cons cat cat-list-no-dates)))
                               (setq cat-list (cdr cat-list))))
                           (mapconcat 
                            (function (lambda (x) 
                                        (when x (replace-regexp-in-string "\\(\\[\\[\\|\\]\\]\\)" "" x)))) 
                            cat-list-no-dates ", "))
                  (planner-task-plan task)))
               (task-due 
                (when task-date (concat "\nDUE:" (icalendar--date-to-isodate task-date))))
               (contents
                (concat task-due
                        "\nSUMMARY:" (planner-task-description task)
                        "\nCATEGORIES:" task-categories)))
												 
          (setq result (concat result header contents "\nEND:VTODO"))))
      (setq tasks (cdr tasks)))
    (let ((coding-system-for-write 'utf-8))
      (insert "BEGIN:VCALENDAR")
      (insert "\nPRODID:-//Emacs//NONSGML planner-ical.el//EN")
      (insert "\nVERSION:2.0")
      (when result (insert result))
      (insert "\nEND:VCALENDAR\n"))))

(defun planner-ical-export-this-page ()
  "Display the tasks on the current page in iCal format."
  (interactive)
  (if (planner-derived-mode-p 'planner-mode)
      (planner-ical-export-page (planner-page-name))
    (error "Not in planner page")))

(provide 'planner-ical)

;;; planner-ical.el ends here
