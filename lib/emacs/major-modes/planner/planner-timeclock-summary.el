;;; planner-timeclock-summary.el --- timeclock summary for the Emacs planner
;;

;; Keywords: emacs planner timeclock report summary
;; Author: Dryice Liu <dryice AT liu DOT com DOT cn>
;; Time-stamp: <16/07/2005 21:43:50 Yann Hodique>
;; Description: Summary timeclock of a day

;; This file is not part of GNU Emacs.

;; Copyright (C) 2004 Dryice Dong Liu . All rights reserved.
;; Parts copyright (C) 2004 Chris Parsons (chris.p AT rsons.org)

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
;; planner-timeclock-summary.el produces timeclock reports for planner
;; files.
;;
;; There are two ways you can use it:
;;
;; 1. Display a temporary buffer
;;
;; Call `planner-timeclock-summary-show' and Emacs will ask you which
;; day's summary do you want. Choose the date as anywhere else of
;; Emacs planner, and a tempory buffer will be displayed with the
;; timeclock summary of that day.
;;
;; 2. Rewrite sections of your planner
;;
;; Choose this approach if you want timeclock summary to be in their
;; own section and you would like them to be readable in your plain
;; text files even outside Emacs. Caveat: The timeclock summary
;; section should already exist in your template and will be rewritten
;; when updated. Tip: Add `planner-timeclock-summary-section'
;; (default: "Timeclock") to your `planner-day-page-template'.
;;
;; To use, call `planner-timeclock-summary-update' in the planner day
;; page to update the section. If you want rewriting to be
;; automatically performed, call `planner-timeclock-summary-insinuate'
;; in your .emacs file

;;; REQUIRE
;; to make a nice text table, you need align.el from
;; http://www.newartisans.com/johnw/Emacs/align.el

;;; TODO
;; - sort?

(require 'planner-timeclock)
(require 'align)
(require 'time-date)

(eval-and-compile
  ;; Workaround for Win2k time-date.el bug reported by David Lord
  (unless (fboundp 'time-subtract)
    (defalias 'time-subtract 'subtract-time)))

;; User functions:
;; planner-timeclock-summary-insinuate
;; planner-timeclock-summary-update
;; planner-timeclock-summary-show
;; planner-timeclock-summary-show-range
;; planner-timeclock-summary-show-filter
;; planner-timeclock-summary-show-range-filter

;;; Code:

;;;_+ User variables

(defgroup planner-timeclock-summary nil
  "Timeclock reports for planner.el."
  :prefix "planner-timeclock-summary"
  :group 'planner)

(defcustom planner-timeclock-summary-section "Timeclock"
  "Header for the timeclock summary section in a plan page."
  :type 'string
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-buffer "*Planner Timeclock Summary*"
  "Buffer name for timeclock reports from `planner-timeclock-summary-show'."
  :type 'string
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-not-planned-string "Not Planned"
  "Project name for `timeclock-in' tasks without a project name."
  :type 'string
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-placeholder-char "."
  "Placeholder for blank cells in the report table.
If there are have blank cells in a simple table, the generated HTML
table will mess up. This character will be used as a placeholder."
  :type 'character
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-include-sub-plan-pages-flag nil
  "Non-nil means include 'sub plan pages' when doing plan page reports.

If non-nil, when updating timeclock reports on plan pages we will
also include plan pages which have this page's name as a prefix. If
nil, only exact matches will be included.

For example: if nil, on a plan page called 'Personal' we would only
include timeclock data marked as 'Personal' (this is the default
behaviour). If non-nil, we would additionally include
'PersonalHomework', 'PersonalYodeling' etc."
  :type 'boolean
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-summary-string
  "\n\nDay began: %B, Day ended: %E\nTime elapsed: %S, \
Time clocked: %C\nTime clocked ratio: %R\n"
  "The string below the report table.

%B the first time checked in the day
%L the last time checked in the day
%E the last time checked in the day, or the current time if it's today
%s span, the difference between %B and %L
%S span, the difference between %B and %E
%C the total time clocked
%r clocked/%s
%R clocked/%S"
  :type 'string
  :group 'planner-timeclock-summary)

;;;_+ Internal variables and utility functions

(defvar planner-timeclock-summary-empty-cell-string "====="
  "Internal use, don't touch.")

(defvar planner-timeclock-summary-total-cell-string "======="
  "Internal use, don't touch.")

(defun planner-timeclock-within-date-range (start-date end-date test-date)
  "Return non-nil if START-DATE and END-DATE contain TEST-DATE.
Dates should be of the form YYYY/MM/DD or YYYY.MM.DD."
  (not (or (string< test-date start-date)
           (string< end-date test-date))))

;;;_+ Data extraction

(defun planner-timeclock-summary-day-range-entry (start-date end-date &optional filter)
  "Return the data between START-DATE and END-DATE (inclusive)
START-DATE and END-DATE should be strings of the form YYYY/MM/DD.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil.

Use the format specified in timeclock.el."
  (let ((day-list (timeclock-day-alist))
        entry-list
        item
        day)
    (while day-list
      (setq day (car day-list))
      (setq day-list (cdr day-list))
      (when (and (or (not start-date)
                     (planner-timeclock-within-date-range start-date end-date (car day))))
        (setq entry-list (append (cddr day) entry-list))))
    (unless (or (null filter)
                (string= filter ""))
      (setq entry-list
            (delq nil
                  (mapcar
                   (lambda (item)
                     (when (cond
                            ((stringp filter)
                             (and (nth 2 item) (string-match filter (nth 2 item))))
                            ((functionp filter)
                             (funcall filter item)))
                       item))
                   entry-list))))
    (setq entry-list
          (cons (cond ((not start-date) (prin1-to-string filter))
                      ((string= start-date end-date) start-date)
                      (t (concat start-date " - " end-date)))
                entry-list))))

(defun planner-timeclock-summary-one-day-entry (date)
  "Return the data associated with DATE.
DATE should be a string of the form YYYY/MM/DD."
  (planner-timeclock-summary-day-range-entry date date))

(defun planner-timeclock-summary-one-day-entry-no-date (date)
  "Return the entries for DATE.
DATE should be a string of the form YYYY/MM/DD."
  (let ((entry-list (planner-timeclock-summary-day-range-entry date date)))
    (cdr entry-list)))

(defun planner-timeclock-summary-one-day-alist (date)
  "Return the entries for DATE as an alist.
DATE should be a string of the form YYYY/MM/DD."
  (let ((entry-list (planner-timeclock-summary-day-range-entry date date)))
    (cddr entry-list)))

(defun planner-timeclock-summary-day-range-alist (start-date end-date)
  "Return the entries between START-DATE and END-DATE (inclusive) as an alist.
START-DATE and END-DATE should be strings of the form YYYY/MM/DD."
  (let ((entry-list (planner-timeclock-summary-day-range-entry start-date end-date)))
    (cddr entry-list)))

(defun planner-timeclock-summary-extract-data (data-list)
  "Return the timeclock data for dates included in DATA-LIST."
  (with-planner
    (let (target-data task-data entry)
      (while data-list
        (setq entry (car data-list))
        (setq data-list (cdr data-list))
        (setq task-data (planner-timeclock-task-info entry))
        (let* ((plan (planner-timeclock-task-plan task-data))
               (entry-project-name (if plan
                                       (planner-link-base plan)
                                     planner-timeclock-summary-not-planned-string))
               (entry-task-name (planner-timeclock-task-description task-data))
               (entry-task-length (planner-timeclock-task-length task-data)))
          ;; total time
          (if target-data
              (setcar target-data (+ (car target-data) entry-task-length))
            (setq target-data (list entry-task-length)))
          ;; updating project
          (let ((projects (cdr target-data))
                project-found
                project)
            (while projects
              (setq project (car projects))
              (let ((project-name (caar project))
                    (project-time (car (cdar project))))
                (if (and project-name
                         (string-equal project-name entry-project-name))
                    ;; the same project has been recorded, updating tasks
                    (let ((tasks (cdr project))
                          task-found
                          task)
                      (while tasks
                        (setq task (car tasks))
                        (let ((task-name (car (cdr (cdr task)))))
                          (if (and task-name
                                   (string-equal task-name
                                                 entry-task-name))
                              ;; the same task has been recorded, add
                              ;; time
                              (progn
                                (setcar task (+ (car task)
                                                entry-task-length))
                                (setq tasks nil)
                                (setq task-found t))
                            (setq tasks (cdr tasks)))))
                      ;; make a new task record
                      (if (not task-found)
                          (setcar projects
                                  (add-to-list 'project (list entry-task-length
                                                              0
                                                              entry-task-name) t)))
                      ;; update project time
                      (setcar (cdar project) (+ project-time
                                                entry-task-length))
                      (setq projects nil)
                      (setq project-found t))
                  (setq projects (cdr projects)))))
            ;; make a new project record
            (if (not project-found)
                (add-to-list 'target-data (list (list entry-project-name
                                                      entry-task-length
                                                      0)
                                                (list entry-task-length
                                                      0
                                                      entry-task-name)) t)))))
      target-data)))

(defun planner-timeclock-summary-extract-data-day (date)
  "Prepare the data for the summary for DATE.
Read `timeclock-file' and return an alist. The list will be of the form:
  (TotalTime
   (((Project1Name Project1Time Project1Ratio) (p1t1time p1t1ratio p1t1name)
                                            (p1t2time p1t2ratio p1t2name)
					    ...)
   ((p2name p2time p2ratio) ...)))"
  (planner-timeclock-summary-extract-data
   (planner-timeclock-summary-one-day-alist date)))

(defun planner-timeclock-summary-make-summary-string-range
  (start-date end-date total &optional filter)
  "Use `planner-timeclock-summary-summary-string' from START-DATE to END-DATE.
Dates are in format YYYY/MM/DD. TOTAL is the total time clocked
today, in seconds.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil."
  (let* ((target-string planner-timeclock-summary-summary-string)
         (data (planner-timeclock-summary-day-range-entry start-date end-date filter))
         begin end last span2 span
         (case-fold-search nil))
    (setq begin (timeclock-day-begin data))
    (setq last (timeclock-day-end data))
    (if (string-equal end-date (format-time-string "%Y/%m/%d"))
        (setq end (current-time))
      (setq end last))
    (setq span (timeclock-time-to-seconds (time-subtract last begin)))
    (setq span2 (timeclock-time-to-seconds (time-subtract end begin)))
    (mapcar
     (lambda (replacement)
       (setq target-string
             (planner-replace-regexp-in-string
              (car replacement)
              (cdr replacement)
              target-string
              t t)))
     (list (cons "%B" (format-time-string "%H:%M:%S" begin))
           (cons "%E" (format-time-string "%H:%M:%S" end))
           (cons "%L" (format-time-string "%H:%M:%S" last))
           (cons "%s" (timeclock-seconds-to-string span t))
           (cons "%S" (timeclock-seconds-to-string span2 t))
           (cons "%C" (timeclock-seconds-to-string total t))
           (cons "%r" (format "%2.1f%%" (* 100 (/ total span))))
           (cons "%R" (format "%2.1f%%" (* 100 (/ total span2))))))
    target-string))

(defun planner-timeclock-summary-make-summary-string (date total)
  "Convenience function for getting the summary string for DATE.
DATE is in the form YYYY/MM/DD. TOTAL is the total time clocked
today, in seconds."
  (planner-timeclock-summary-make-summary-string-range date date total))

(defun planner-timeclock-summary-calculate-ratio-day (start-date &optional end-date filter)
  "Calculate time ratio for START-DATE to END-DATE.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil."
  (when (not end-date)
    (setq end-date start-date))
  (let (target-data)
    (setq target-data (planner-timeclock-summary-extract-data
                       (cdr (planner-timeclock-summary-day-range-entry
                             start-date end-date filter))))
    (let ((total (car target-data))
          (projects (cdr target-data)))
      (while projects
        (let ((project (car projects))
              (tasks (cdar projects)))
          (setcar (cdr (cdar project)) (/ (car (cdar project)) total))
          (while tasks
            (let ((task (car tasks)))
              (setcar (cdr task) (/ (car task) total))
              (setq tasks (cdr tasks))))
          (setq projects (cdr projects)))))
    target-data))

;;;_+ Presentation

(defun planner-timeclock-summary-make-text-table-day
  (start-date &optional end-date filter hide-summary)
  "Make the summary table for START-DATE to END-DATE using plain text.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current item, and the line considered if the function
returned non-nil.

If START-DATE is nil, then it will ignore the date information
and return data for everything. If HIDE-SUMMARY is non-nil, do
not include the summary."
  (unless end-date (setq end-date start-date))
  (let (source-list)
    (setq source-list (planner-timeclock-summary-calculate-ratio-day
                       start-date end-date filter))
    (let ((projects (cdr source-list))
          (total (car source-list))
          (project-name-format "20.20s"))
      (if total
          (with-temp-buffer
            (erase-buffer)
            (insert (format (concat "%-" project-name-format "|%9s|%6s| %s\n") "Project" "Time" "Ratio" "Task"))
            (while projects
              (let ((project-data (caar projects))
                    (tasks (cdar projects))
                    task)
                (insert
                 (if (or (null (car project-data))
                         (string= (car project-data) planner-timeclock-summary-not-planned-string))
                     (format (concat "%-" project-name-format "|") planner-timeclock-summary-not-planned-string)
                  (format (concat "[[%s][%-" project-name-format "]]|")
                          (planner-link-base (car project-data))
                          (planner-link-name (car project-data)))))
                (setq task (car tasks))
                (insert (format "%9s|%5s%%| %s\n"
                                (timeclock-seconds-to-string (car task) t)
                                (format "%2.1f" (* 100 (cadr task)))
                                (car (cddr task))))
                (setq tasks (cdr tasks))
                (while tasks
                  (let ((task (car tasks)))
                    (insert (format (concat "%-" project-name-format "|%9s|%5s%%| %s\n")
                                    planner-timeclock-summary-empty-cell-string
                                    (timeclock-seconds-to-string (car task) t)
                                    (format "%2.1f" (* 100 (cadr task)))
                                    (car (cddr task))))
                    (setq tasks (cdr tasks))))
                (insert (format "%s|%9s|%5s%%| %s\n"
                                planner-timeclock-summary-total-cell-string
                                (timeclock-seconds-to-string (cadr project-data) t)
                                (format "%2.1f" (* 100 (nth 2 project-data)))
                                planner-timeclock-summary-empty-cell-string))
                (setq projects (cdr projects))))
;            (align-regexp (point-min) (point-max) "\\(\\s-*\\)|" 1 0 nil)
            (untabify (point-min) (point-max))
            (goto-char (point-min))
            (let ((total-regexp (format "%s\\s-*|"
                                        planner-timeclock-summary-total-cell-string)))
              (while (search-forward-regexp total-regexp nil t)
                (let ((string-len (- (match-end 0) (match-beginning 0))))
                  (replace-match (format (concat "%" project-name-format "|")
                                         "Total:")))))
            (goto-char (point-min))
            (while (search-forward
                    planner-timeclock-summary-empty-cell-string nil t)
              (replace-match (concat
			      planner-timeclock-summary-placeholder-char
			      (make-string
			       (- (length planner-timeclock-summary-empty-cell-string)
				  1)
                              (aref " " 0)))))
            (goto-char (point-max))
            (unless hide-summary
              (insert (planner-timeclock-summary-make-summary-string-range
                       start-date end-date total filter)))
            (buffer-string))
        ""))))

;;;###autoload
(defun planner-timeclock-summary-insinuate ()
  "Automatically call `planner-timeclock-summary-update'.
This function is called when the day page is saved."
  (add-hook 'planner-mode-hook
            (lambda ()
              (add-hook
               (cond
                ((boundp 'write-file-functions) 'write-file-functions)
                ((boundp 'local-write-file-hooks) 'local-write-file-hooks)
                ((boundp 'write-file-hooks) 'write-file-hooks))
               'planner-timeclock-summary-update nil t))))

;;;###autoload
(defun planner-timeclock-summary-update ()
  "Update `planner-timeclock-summary-section'. in the current day page.
The section is updated only if it exists."
  (interactive)
  (save-excursion
    (save-restriction
      (when (planner-narrow-to-section planner-timeclock-summary-section)
        (delete-region (point-min) (point-max))
        (let ((thepage (planner-page-name)))
          (insert "* " planner-timeclock-summary-section "\n\n"
                  (if (and thepage (string-match planner-date-regexp thepage))
                      (planner-timeclock-summary-make-text-table-day
                       (planner-replace-regexp-in-string "\\." "/"
                                                         thepage t t))
                    (planner-timeclock-summary-make-text-table-day
                     nil nil
                     (concat "^" thepage
                             (unless planner-timeclock-summary-include-sub-plan-pages-flag ":"))
                     t))
                  " \n"))))))

;;;###autoload
(defun planner-timeclock-summary-show (&optional date)
  "Display a buffer with the timeclock summary for DATE.
Date is a string in the form YYYY.MM.DD."
  (interactive (list (planner-read-date)))
  (planner-timeclock-summary-show-range date date))

;;;###autoload
(defun planner-timeclock-summary-show-filter (filter date)
  "Show a timeclock report filtered by FILTER for DATE.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil.

If called interactively, prompt for FILTER (a regexp) and DATE.
DATE is a string in the form YYYY.MM.DD and can be nil."
  (interactive
   (list
    (read-string "Filter (regexp): " nil 'regexp-history)
    (planner-read-date)))
  (planner-timeclock-summary-show-range date date filter))

;;;###autoload
(defun planner-timeclock-summary-show-range-filter (filter start-date end-date)
  "Show a timeclock report filtered by FILTER for START-DATE to END-DATE.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil.

If called interactively, prompt for FILTER (a regexp), START-DATE and END-DATE.
Dates are strings in the form YYYY.MM.DD and can be nil."
  (interactive
   (list
    (read-string "Filter (regexp): " nil 'regexp-history)
    (planner-read-date "Start")
    (planner-read-date "End")))
  (planner-timeclock-summary-show-range start-date end-date filter))

(defun planner-timeclock-summary-show-range (start-date end-date &optional filter)
  "Show a timeclock report for the date range START-DATE to END-DATE.

If FILTER is a regexp, only plan pages matching that regexp will
be included. If FILTER is a function, it will be called with the
current timeclock item, and the line considered if the function
returned non-nil.

Dates are strings in the form YYYY.MM.DD and can be nil."
  (interactive (list (planner-read-date "Start") (planner-read-date "End")))
  (switch-to-buffer (get-buffer-create planner-timeclock-summary-buffer))
  (erase-buffer)
  (let ((emacs-wiki-project planner-project))
    (insert "Timeclock summary report for "
            (if (string-equal start-date end-date)
                start-date
              (concat start-date " - " end-date))
            "\n\n"
            (planner-timeclock-summary-make-text-table-day
             (planner-replace-regexp-in-string "\\." "/" start-date t t)
             (planner-replace-regexp-in-string "\\." "/" end-date t t)
             filter))
    (planner-mode))
  (goto-char (point-min)))

;;;_+ experimental code

(defcustom planner-timeclock-summary-task-project-summary-string
  "*Project Summary*"
  "Task name for project summary."
  :type 'string
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-project-column-min-width 22
  "Minimum width of the project column in the report table."
  :type 'integer
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-time-column-min-width 8
  "Minimum width of the time column in the report table."
  :type 'integer
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-ratio-column-min-width 5
  "Minimum width of the ratio column in the report table."
  :type 'integer
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-summary-task-column-min-width 40
  "Minimum width of the task column in the report table."
  :type 'integer
  :group 'planner-timeclock-summary)

(defun planner-timeclock-summary-make-table-day (date start-point)
  "Format `planner-timeclock-summary-make-text-table-day' neatly.
The report is prepared for DATE. START-POINT is not used."
  ;;   (with-temp-buffer
  ;;     (erase-buffer)
  (insert (planner-timeclock-summary-make-text-table-day date))
  ;;     (planner-mode)
  (redraw-display)
  (table-capture
   42 (point-max) "|" "\n" 'left
   (list planner-timeclock-summary-project-column-min-width
         planner-timeclock-summary-time-column-min-width
         planner-timeclock-summary-ratio-column-min-width
         planner-timeclock-summary-task-column-min-width))
  ;; make "=====" cell empty and span above
  ;;     (goto-char (point-min))
  ;;     (while (search-forward
  ;; 	    planner-timeclock-summary-empty-cell-string)
  ;;       (beginning-of-line)
  ;;       (kill-line)
  ;;       (table-span-cell 'above))
  ;;     (buffer-string))
  )

;;;###autoload
(defun planner-timeclock-summary-show-2 (&optional date)
  "Display a buffer with the timeclock summary for DATE.

Date is a string in the form YYYY.MM.DD. It will be asked if not
given."
  (interactive (planner-read-date))
  (switch-to-buffer (get-buffer-create planner-timeclock-summary-buffer))
  (erase-buffer)
  (let ((emacs-wiki-project planner-project))
    (insert "Timeclock summary report for " date "\n\n")
    (planner-mode)
    (planner-timeclock-summary-make-table-day
     (planner-replace-regexp-in-string "\\." "/" date t t) (point)))
  (goto-char (point-min)))

(defun planner-timeclock-summary-table-span-cell-left ()
  "Merge the current cell with the one to the left."
  (table-span-cell 'left))

(defun planner-timeclock-summary-table-span-cell-above ()
  "Merge the current cell with the one above it."
  (table-span-cell 'above))

(provide 'planner-timeclock-summary)
;;; planner-timeclock-summary.el ends here

