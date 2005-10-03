;;; planner-schedule.el --- Schedule integration for the Emacs Planner

;; Copyright (C) 2001 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Version: 2005.08.20-17.59-stable
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

;; This module allows you to project task completion time.
;; Tasks should be of the form

;; #A0 _ 2h Do something
;; #B0 _ 1h30m Do something
;; #B0 _ 2d Do something
;; #B0 _ 2w Do something
;; #B0 _ 10s Do something

;; s: seconds, m: minutes, h: hours, d: days, w: weeks

;; You can get schedule.el from
;; http://www.newartisans.com/johnw/Emacs/schedule.el

;;; Code:

(require 'planner)
(require 'schedule)

(defun planner-schedule-task-estimate (info)
  "Return a time estimate for task (INFO) completion, in seconds."
  (when (string-match "\\`\\s-*\\([0-9]+[smhdw]\\)"
                      (planner-task-description info))
    (schedule-duration-to-seconds
     (match-string 1 (planner-task-description info)))))

(defun planner-schedule-end-projection ()
  "Show when today's task load will be finished, according to estimates."
  (require 'schedule)
  (schedule-initialize)
  (save-excursion
    (let ((now (schedule-completion-time (current-time) 0))
          spent remaining slippage finish)
      (goto-char (point-min))
      (while (re-search-forward "^#[A-C]" nil t)
        (let* ((task (planner-current-task-info))
               (estimate (planner-schedule-task-estimate task)))
          (when estimate
            (setq now (schedule-completion-time now estimate)))))
      now)))

;;;###autoload
(defun planner-schedule-show-end-project ()
  "Display the estimated project completion time."
  (interactive)
  (message (format-time-string "%c" (planner-schedule-end-projection))))

(condition-case nil
    (let ((map planner-mode-map))
      (define-key map (kbd "C-c RET") 'planner-schedule-show-end-project)
      (define-key map "\C-c\C-m" 'planner-schedule-show-end-project)
      (if (featurep 'xemacs)
          (define-key map (kbd "C-c C-T C-e")
            'planner-schedule-show-end-project)
        (define-key map (kbd "C-c C-S-t C-e")
          'planner-schedule-show-end-project)))
  (error (message "Could not bind schedule keys in planner mode")))

(provide 'planner-schedule)

;;; planner-schedule.el ends here
