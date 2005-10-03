;;; planner-report.el --- create a timely status report based on planner pages

;; Copyright 2004 by Andrew J. Korty <ajk@iu.edu>

;; Emacs Lisp Archive Entry
;; Filename: planner-authz.el
;; Version: $Revision$
;; Keywords: hypermedia
;; Author: Andrew J. Korty <ajk@iu.edu>
;; Maintainer: Andrew J. Korty <ajk@iu.edu>
;; Description: Create a timely status report based on planner pages
;; URL:
;; Compatibility: Emacs21

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

;; This library creates a status report for a given timespan.  The
;; report itself is just another emacs-wiki page in your planner
;; directory.  Once generated, it contains tasks and notes culled from
;; active project pages.  Tasks are only shown if they are incomplete
;; or were completed within the timespan.  Notes are shown if they
;; were created during the timespan.  Tasks and notes are grouped
;; together under a heading for their corresponding project.

;; The idea is you have one of these status reports generated
;; periodically (say, every couple of weeks).  Perhaps you use cron to
;; run them automatically and then mail you a reminder that they've
;; been done.  Then you can edit the page, adding verbiage where it is
;; needed and removing irrelevant items.  This editing process is as
;; easy as editing any other emacs-wiki page.  Finally, you can
;; publish the page along with the rest of your planner using
;; M-x emacs-wiki-publish.

;; If you use planner-authz.el, you can tell planner-report.el only to
;; consult project pages that a given list of users
;; (planner-report-authz) can access when generating the report.  For
;; example, if you're preparing a status report for your boss, add
;; yourself and him to planner-report-authz.  The resulting status
;; report will only contain information the two of you are supposed to
;; have access to, and the report itself will be similarly restricted.

;; * Startup and Usage

;; Once this file is installed, add the following to your .emacs file:

;;   (require 'planner-report)

;; Then you can use the following command to generate a status report:

;;   M-x planner-report-generate

;; You will be prompted for a beginning and ending date, and then the
;; status report will be generated.  You can then edit it to your
;; liking and publish it just like you would the rest of your planner.

;; * Customization

;; All user-serviceable options can be customized with
;; M-x customize-group RET planner-report RET.

;; $Id$

;;; Code:

(defgroup planner-report nil
  "A planner.el extension for generating timely status reports
based on planner pages."
  :group 'planner
  :prefix "planner-report")

(defcustom planner-report-authz nil
  "List of users a status report should be restricted to.
When status reports are generated, only planner pages accessible
by these users will be consulted, and the resulting status report
will be similarly restricted."
  :group 'planner-report
  :type '(repeat string))

(defcustom planner-report-remove-task-numbers t
  "Remove task numbers when generating status reports."
  :group 'planner-report
  :type 'boolean)

(defcustom planner-report-replace-note-numbers "**"
  "If non-nil, a string with which to replace note numbers when
generating status reports."
  :group 'planner-report
  :type 'string)

(defcustom planner-report-unfinished-offset nil
  "If non-nil, the offset in days from the current date of
unfinished tasks to include in the status report.  If nil,
include all unfinished tasks."
  :group 'planner-report
  :type '(choice (integer :tag "Number of days")
		 (const :tag "Include all unifinished tasks" nil)))

(defvar planner-report-version "$Revision$"
  "Version of of planner-report.el.")

;;;###autoload
(defun planner-report-generate (begin end)
  "Generate a status report spanning a period from BEGIN to END.
BEGIN and END are in the format YYYY.MM.DD."
  (interactive
   (let ((planner-expand-name-favor-future-p
          (or planner-expand-name-favor-future-p
              planner-task-dates-favor-future-p)))
     (list (planner-read-date "Start date")
           (planner-read-date "End date"))))
  (save-some-buffers nil (lambda () (planner-derived-mode-p 'planner-mode)))
  (cd (planner-directory))
  (let ((filename (concat "StatusReport" end)))
    (with-temp-buffer
      (when planner-report-authz
        (require 'planner-authz)
        (insert "#authz "
                (mapconcat 'identity planner-report-authz " ") "\n"))
      (insert "#title Status report for " begin " to " end "\n")
      (let ((pages (if planner-report-authz
                       (planner-authz-file-alist planner-report-authz)
                     (planner-file-alist)))
            notes tasks)
        (while pages
          (when (caar pages)
            ;; Add only project pages, and skip other status reports
            (unless (or (string-match planner-name-regexp (caar pages))
                        (string-match "^StatusReport" (caar pages)))
              (with-temp-buffer
                (with-planner
                  (insert-file-contents-literally (cdar pages))
                  (setq tasks
                        (planner-report-find-tasks (caar pages) begin end))
                  (setq notes
                        (planner-report-find-notes (caar pages) begin end)))))
            ;; Insert a linked heading if we found anything
            (if (or notes tasks)
                (insert "\n* [[" (caar pages) "]["
                        (or (emacs-wiki-get-title-fast (cdar pages))
                            (emacs-wiki-prettify-title (caar pages)))
                        "]]\n\n"))
            (when tasks
              (insert tasks "\n\n")
              (setq tasks nil))
            (when notes
              (insert notes "\n")
              (setq notes nil)))
          (setq pages (cdr pages))))
      (write-file filename t))
    (find-file filename)))

(defun planner-report-find-notes (page begin end)
  "Find notes on PAGE that were created between BEGIN and END.
BEGIN and END are formatted as YYYY.MM.DD."
  (goto-char (point-min))
  (let (result)
    (while (re-search-forward "^\\.#[0-9]+\\s-+" nil t)
      (let ((note
              (buffer-substring
               (planner-line-beginning-position)
               (save-excursion
                 ;; Find the end of this note (maybe EOF)
                 (re-search-forward "^\\(\\.#[0-9]+\\s-+\\|\\*\\*?\\s-+\\)"
                                    nil 1)
                 (goto-char (planner-line-beginning-position))
                 (point))))
             (info (planner-current-note-info)))
        (when info
          (let* ((link (planner-note-link info))
                 (date (if link (emacs-wiki-wiki-base link))))
            ;; Snarf if note is associated with a date that is in range
            (and date
                 (not (string< date begin))
                 (not (string< end date))
                 (progn
                   (if planner-report-replace-note-numbers
                       (setq note
                             (planner-replace-regexp-in-string
                              "^\\.#[0-9]+"
                              planner-report-replace-note-numbers
                              note t t)))
                   (setq result (if result (concat note result) note))))))))
    result))

(defun planner-report-find-tasks (page begin end)
  "Find cancelled or completed tasks on PAGE with a date between
BEGIN and END and any unfinished tasks with a date constrained by
`planner-report-unfinished-offset'.  BEGIN and END are formatted
as YYYY.MM.DD."
  (goto-char (point-min))
  (let (result)
    (while (re-search-forward "^#[A-C]" nil t)
      (let* ((task (buffer-substring (planner-line-beginning-position)
                                     (planner-line-end-position)))
             (info (planner-task-info-from-string page task)))
        (when info
          (let ((date (planner-task-date info)))
            ;; If the task isn't cancelled nor completed and has a
            ;; date less than or equal to planner-report-unfinished
            ;; away, snarf.  If it has been cancelled or completed and
            ;; the date is in range, snarf.
            (and date
                 (or (and (not (or (equal (planner-task-status info) "C")
                                   (equal (planner-task-status info) "X")))
                          (or (null planner-report-unfinished-offset)
                              (not (string<
                                    (planner-calculate-date-from-day-offset
                                     (planner-date-to-filename
                                      (decode-time (current-time)))
                                     planner-report-unfinished-offset)
                                    date))))
                     (and (not (string< date begin))
                          (not (string< end date))))
                 (progn
                   (if planner-report-remove-task-numbers
                       (setq task (planner-replace-regexp-in-string
                                   "^\\(#[A-C]\\)\\([0-9]+ +\\)"
                                   "\\1 " task t t)))
                   (setq result
                         (if result (concat result "\n" task) task))))))))
    result))

(provide 'planner-report)

;;; planner-report.el ends here
