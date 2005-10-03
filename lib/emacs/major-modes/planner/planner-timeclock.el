;;; planner-timeclock.el --- Timeclock integration for the Emacs Planner

;; Copyright (C) 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Keywords: planner, timeclock
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php#timeclock

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

;; This module allows you to clock in and clock out of your projects.
;; You can also generate reports with the <timeclock-report> tag.
;; timeclock.el is part of GNU Emacs and can be downloaded from
;;
;;    http://sacha.free.net.ph/notebook/emacs/timeclock.el

;;; Code:

(require 'planner)
(require 'timeclock)
(require 'planner-schedule)
(require 'easymenu)

(defvar planner-timeclock-current-task nil
  "Description of current task.")

(defalias 'planner-timeclock-in 'planner-task-in-progress)

(defadvice timeclock-out (after planner-timeclock activate)
  "Clear `planner-timeclock-current-task.'"
  (setq planner-timeclock-current-task nil))

(defun planner-timeclock-plan-string (task-info &optional plans)
  "Return the string for the plan part of the timeclock entry for TASK-INFO."
  (if (or plans (planner-task-plan task-info))
      (concat
       (if (featurep 'planner-multi)
           ;; Remove the date links
           (planner-multi-make-link
            (planner-multi-filter-links
             planner-date-regexp
             (if plans
                 (planner-multi-split plans)
               (planner-multi-task-link-as-list task-info))
             t))
         ;; Single link, no problem
         (or plans (planner-task-plan task-info)))
       ": ")
    ""))

(defun planner-timeclock-task-marked (old-status new-status)
  "Clock out if the task description matches the one clocked in."
  (cond
   ((string= new-status "X")
    (when (and planner-timeclock-current-task
               (string= (planner-task-description (planner-current-task-info))
                        planner-timeclock-current-task)
               (timeclock-currently-in-p))
      (timeclock-out 1)))
   ((string= new-status "P")
    (when (and planner-timeclock-current-task
             (string= (planner-task-description (planner-current-task-info))
                      planner-timeclock-current-task)
             (timeclock-currently-in-p))
    (timeclock-out)))
   ((string= new-status "o")
    (let* ((task-info (planner-current-task-info))
           (project (concat
                     (planner-timeclock-plan-string task-info)
                     (planner-task-description task-info))))
      (if (timeclock-currently-in-p)
          (timeclock-change nil project)
        (timeclock-in nil project))
      (setq planner-timeclock-current-task (planner-task-description task-info)))))
  t)
(add-hook 'planner-mark-task-hook 'planner-timeclock-task-marked)

(defadvice planner-replan-task (around planner-timeclock activate)
  "Edit the clocked in task as well."
  (let ((info (planner-current-task-info)))
    ad-do-it
    (with-current-buffer (find-file-noselect timeclock-file)
      (goto-char (point-min))
      (while (re-search-forward
              (concat
               "^. [^ ]+ [^ ]+ "
               "\\(" (regexp-quote (planner-timeclock-plan-string info)) "\\)"
               (regexp-quote (planner-task-description info)) "$") nil t)
        (replace-match
         (save-match-data (planner-timeclock-plan-string nil (ad-get-arg 0)))
         t t nil 1))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defadvice planner-edit-task-description (around planner-timeclock activate)
  "Update the timelog as well. Warning! Do not have duplicate tasks!"
  (let ((info (planner-current-task-info)))
    (when (string= (planner-task-description info) planner-timeclock-current-task)
      (setq planner-timeclock-current-task (ad-get-arg 0)))
    ad-do-it
    (with-current-buffer (find-file-noselect timeclock-file)
      (goto-char (point-min))
      (while (re-search-forward
              (concat
               "^. [^ ]+ [^ ]+ "
               (regexp-quote (planner-timeclock-plan-string info))
               "\\("
               (regexp-quote (planner-task-description info))
               "\\)$")
              nil t)
        (replace-match (ad-get-arg 0) t t nil 1))
      (save-buffer)
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun planner-timeclock-report-tag (beg end highlight-p)
  "Replace the region BEG to END with a timeclock report.
If HIGHLIGHT-P is non-nil, the output is also displayed."
  (require 'timeclock)
  (if highlight-p
      (add-text-properties
       beg end (list 'display
                     (with-temp-buffer
                       (timeclock-generate-report emacs-wiki-publishing-p)
                       (buffer-string))))
    (delete-region beg end)
    (timeclock-generate-report emacs-wiki-publishing-p)
    (add-text-properties beg (point) '(read-only t))))

(add-to-list 'planner-markup-tags
             '("timeclock-report" nil nil t planner-timeclock-report-tag))
(planner-option-customized 'planner-markup-tags planner-markup-tags)

(let ((map planner-mode-map))
  (define-key map "\C-c\C-i" 'planner-task-in-progress)
  (define-key map "\C-c\C-o" 'timeclock-out)
  (if (featurep 'xemacs)
      (progn
        (define-key map (kbd "C-c C-T C-i") 'planner-task-in-progress)
        (define-key map (kbd "C-c C-T C-o") 'timeclock-out))
    (define-key map (kbd "C-c C-S-t C-i") 'planner-task-in-progress)
    (define-key map (kbd "C-c C-S-t C-o") 'timeclock-out)))

(defun planner-timeclock-task-plan (info)
  "Return the first plan page associated with INFO."
  (car (elt info 0)))

(defun planner-timeclock-task-plan-as-list (info)
  "Return all the plan pages associated with INFO."
  (elt info 0))

(defun planner-timeclock-task-description (info)
  "Return the descrption associated with INFO."
  (elt info 1))

(defun planner-timeclock-task-length (info)
  "Return the length associated with INFO."
  (elt info 2))

(defun planner-timeclock-task-info (entry)
  "Parse ENTRY and return a list of the form (plan task length).
See `timeclock-log-data' for the format of ENTRY. Note that the
project field in `timeclock-log-data' is 'project: task' here."
   (let ((project (if (stringp entry) entry (timeclock-entry-project entry)))
         plan task)
    (when project
      (with-planner
        (cond
         ;; No plan, just the task
         ((string-match "^\\s-*:\\s-+" project) 
          (setq task (substring project (match-end 0))))
         ;; Single link
         ((string-match (concat "^\\(" emacs-wiki-name-regexp "\\): ") project)
          (setq plan (list (match-string 1 project)))
          (setq task (substring project (match-end 0))))
         ;; Multi link
         ((and
           (featurep 'planner-multi)
           (string-match
            (concat "^\\(\\(?:" emacs-wiki-name-regexp "\\)"
                    "\\(?:" planner-multi-separator
                    "\\(?:" emacs-wiki-name-regexp "\\)\\)*\\): ")
            project))
          (setq task (substring project (match-end 0)))
          (setq plan (planner-multi-split (match-string 1 project))))
         ;; Nothing whatsoever.
         (t (setq task project)))
        (list plan task (unless (stringp entry) (timeclock-entry-length entry)))))))

(condition-case nil
    ;; XEmacs seems to be missing this function in some released
    ;; versions of XEmacs21.
    (if (fboundp 'easy-menu-create-menu)
        (easy-menu-add-item planner-mode-map
                            '("Planner")
                            (easy-menu-create-menu
                             "Timeclock"
                             '(["Clock into a task" planner-timeclock-in]
                               ["Clock out" timeclock-out])))
      (easy-menu-add-item planner-menu
                          '("Timeclock")
                          ["Clock into a task" planner-timeclock-in t]
                          "Plan")
      (easy-menu-add-item planner-menu
                          '("Timeclock")
                          ["Clock out" timeclock-out t]
                          "Plan")))

(provide 'planner-timeclock)

;;; planner-timeclock.el ends here
