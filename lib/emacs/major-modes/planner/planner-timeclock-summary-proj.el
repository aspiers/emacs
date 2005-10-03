;;; planner-timeclock-summary-proj.el --- timeclock project for the Emacs planner

;; Copyright (C) 2004 Pascal Quesseveur

;; Author: Pascal Quesseveur <quesseveur@abaksystemes.fr>
;; Time-stamp: <2004-12-17 18:39>
;; Description: Summary timeclock of a project

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
;; planner-timeclock-summary-proj.el produces timeclock reports for planner
;; files.  That package uses `timeclock-project-alist' and
;; `timeclock-seconds-to-string' from timeclock.

;;; To use, call `planner-timeclock-summary-proj-current' from a
;;; project page. The report is inserted at the current position in
;;; the buffer. The function
;;; `planner-timeclock-summary-proj-section' does the same but the
;;; report is inserted inside a section called "* Report".

;;; CODE

(require 'planner-timeclock)
(require 'planner-timeclock-summary)

;;; User variables

;;; Code:
(defcustom planner-timeclock-workdays-per-week 5
  "Number of work days per week."
  :type 'integer
  :group 'planner-timeclock-summary)

(defcustom planner-timeclock-workhours-per-day 8
  "Number of work hours per day."
  :type 'integer
  :group 'planner-timeclock-summary)

;;;; User variables stop here

(defun planner-timeclock-summary-proj-section ()
  "Update the current project's time report in the Report section."
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (re-search-forward "^[*] Report" nil t)
        ;; A report section has been found in the buffer. We delete it.
        (progn
          (forward-line)
          ;; Delete lines until the next section or to end of buffer.
          ;; End of buffer is managed by the (catch throw)
          ;; construct to avoid problems when calling kill-line with
          ;; point at end of buffer ...
          (catch 'loop
            (while (not (looking-at "^[*] "))
              (if (eobp)
                  (throw 'loop nil)
                (kill-line)))))
      ;; section creation when it doesn't exist
      (goto-char (point-max))
      (insert "\n\n* Report\n"))
    ;; insert the report in the buffer
    (planner-timeclock-summary-proj-current)
    (insert "\n\n")))

;;;###autoload
(defun planner-timeclock-summary-proj-all ()
  "Insert time report for all projects in the current buffer."
  (interactive)
  (planner-timeclock-summary-proj-report nil))

;;;###autoload
(defun planner-timeclock-summary-proj-current ()
  "Insert time report for the current project in the current buffer."
  (interactive)
  (let ((project (file-name-nondirectory buffer-file-name)))
    (planner-timeclock-summary-proj-report project)))

;;;###autoload
(defun planner-timeclock-summary-proj-report (project)
  "Insert time report for PROJECT in the current buffer."
  (interactive "sProject: ")
  (insert (planner-timeclock-proj-build-report
           (planner-timeclock-proj-make-alist project))))

(defun planner-timeclock-proj-build-report (proj-alist)
  "Return time report for PROJ-ALIST.
The time report is formatted as

  TASK 0 | duration
  TASK 1 | duration
   TOTAL | duration."
  (let ((str)
        (total-duration 0))
    (while proj-alist
      (let* ((proj-entry (car proj-alist))
             (duration (cdr proj-entry)))
        (setq str
              (concat str "\n" (format "%8s" (car proj-entry)) " | "
                      (planner-timeclock-summary-proj-seconds-to-weekdays-string duration)))
        (setq total-duration (+ duration total-duration))
        (setq proj-alist (cdr proj-alist))))
    (concat str "\n"
            (format "%8s" "TOTAL") " | "
            (planner-timeclock-summary-proj-seconds-to-weekdays-string total-duration) "\n")))

(defun planner-timeclock-proj-make-alist (proj-name)
  "Return an association list for PROJ-NAME.
Each association is of the form (TASK . DURATION). TASK is a task
name defined inside PROJ-NAME and DURATION is the total time
computed for that task. When PROJ-NAME is nil, each TASK is a
project name, and DURATION is the time spent on that project."
  (let ((projects (planner-timeclock-proj-entries proj-name))
        (proj-alist))
    ;; Looping on project data. The project is made of tasks, and for each
    ;; task there can be several time intervals.
    (while projects
      (let* ((entry (car projects))
             (task (car entry))
             (task-data (cdr entry))
             (task-time 0))
        ;; We compute the time spent on task TASK
        (setq task-time 0)
        (while task-data
          (let ((task-entry (car task-data)))
            (progn
              (setq task-time (+ task-time
                                 (timeclock-entry-length task-entry)))
              (setq task-data (cdr task-data)))))
        ;; compute the name
        (if (string-match ": *" task)
            (if (and (< (match-end 0) (length task)) proj-name)
                (setq task (substring task (match-end 0)))
              (setq task (substring task 0 (match-beginning 0)))))
        
        ;; record the cons (task . time)
        (if proj-alist
            (let ((proj-time 0)
                  (proj-data-cell (assoc task proj-alist)))
              (if proj-data-cell
                  (progn
                    (setq proj-time (cdr proj-data-cell))
                    (setcdr proj-data-cell (+ task-time proj-time)))
              (add-to-list 'proj-alist (cons task task-time))))
          (setq proj-alist (list (cons task task-time))))
      (setq projects (cdr projects))))
    proj-alist))

(defun planner-timeclock-proj-entries (proj-name)
  "Return entries from `timeclock-project-alist' for PROJ-NAME.
If PROJ-NAME is nil, return `timeclock-project-alist'."
  (let ((projects)
        (entry-list (timeclock-project-alist)))
    ;; Looping on entries. Each entry is in the form (PROJECT (TASKS
    ;; DATA)). We keep only entries for witch PROJECT-NAME matches
    ;; PROJECT.
    (if (not proj-name)
        entry-list
      (while entry-list
        (let* ((proj (car entry-list))
               (proj-entry-name (car proj)))
          (if (and proj-name
                   (string-match (concat "^" proj-name) proj-entry-name))
              (if projects
                  (add-to-list 'projects proj)
                (setq projects (list proj))))
          (setq entry-list (cdr entry-list))))
      projects)))

(defun planner-timeclock-summary-proj-seconds-to-weekdays-string (seconds)
  "Convert the floating point number SECONDS to a string.
The string is in the form [WWw] [DDd] hh:ss."
  (let* ((workday (* planner-timeclock-workhours-per-day 3600))
         (days (floor (/ seconds workday)))
         (secs (floor (- seconds (* days workday)))))
    (if (> days planner-timeclock-workdays-per-week)
        (let ((weeks (/ days planner-timeclock-workdays-per-week))
              (dys (% days planner-timeclock-workdays-per-week)))
          (if (> dys 0)
              (format "%dw %dd %s" weeks dys
                      (timeclock-seconds-to-string secs))
            (format "%dw %s" weeks
                    (timeclock-seconds-to-string secs))))
      (if (> days 0)
          (format "%dd %s" days
                  (timeclock-seconds-to-string secs))
        (format "%s" (timeclock-seconds-to-string secs))))))

(provide 'planner-timeclock-summary-proj)

;;; planner-timeclock-summary-proj.el ends here


