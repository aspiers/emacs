;;; planner-cyclic.el --- Cyclic task support for the Emacs Planner

;; Copyright (C) 2004 Sandra Jean Chua <sacha@Free.net.ph>

;; Filename: planner-cyclic.el
;; Version: 2005.08.20-17.59-stable
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Provide cyclic task support
;; URL: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/planner-cyclic.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/ChangeLog
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
;;
;; Place planner-cyclic.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-cyclic)
;;
;; Create a diary file named ~/.diary.cyclic-tasks
;; (or the value of planner-cyclic-diary-file). Example:
;;
;;    Tuesday #B0 _ Study Japanese
;;    Friday #B0 _ Study Japanese (JapaneseStudies)
;;
;; The first will be a plain task, the second will be linked.
;;
;; By default, planner-cyclic creates multiple tasks if you let tasks build up
;; (that is, the next Tuesday rolls around and you _still_ haven't
;; marked the task as done.) To turn off this behavior:
;;
;; (setq planner-cyclic-diary-nag nil)

;;; Code:

(require 'planner)
(require 'diary-lib)

(defcustom planner-cyclic-diary-file "~/.diary.cyclic-tasks"
  "Diary file containing cyclic tasks."
  :type 'string
  :group 'planner)

(defcustom planner-cyclic-diary-nag t
  "If non-nil, create tasks even if there are procrastinated cyclic tasks."
  :type 'boolean
  :group 'planner)

;;; functions
(defun planner-cyclic-get-cyclic-tasks (date &optional no-of-days)
  "For DATE, get the cyclic tasks."
  (save-window-excursion
    (save-excursion
      (unwind-protect
          (delq nil
                (let* ((diary-display-hook nil)
                       (diary-file planner-cyclic-diary-file)
                       (list-diary-entries-hook '(include-other-diary-files))
                       (entries (list-diary-entries
                                 (if (stringp date)
                                     (planner-filename-to-calendar-date date)
                                   date) 1)))
                  (mapcar (lambda (item)
                            (when (string-match "#[A-C].+" (elt item 1))
                              (match-string 0 (elt item 1))))
                          entries)))))))

(defun planner-cyclic-generate-task (date task-string)
  "For DATE, generate a cyclic task based on TASK-STRING."
  (let ((info (planner-task-info-from-string date task-string)))
    (if info
        (setcar (nthcdr 4 info)
                (concat (planner-task-description info) " from " date))
      (message "Cannot parse task %s" task-string))
    info))

(defun planner-cyclic-create-task-maybe (date task-string)
  "For DATE, possibly create a task based on TASK-STRING."
  (when (string-match planner-task-regexp task-string)
    (let ((orig-task (planner-task-info-from-string date task-string))
          (new-task (planner-cyclic-generate-task date task-string)))
      (unless (planner-find-task new-task)
        (when (or planner-cyclic-diary-nag (not (planner-find-task orig-task)))
          (planner-create-task-from-info new-task))))))

;;;###autoload
(defun planner-cyclic-create-tasks-maybe ()
  "Maybe create cyclic tasks.
This will only create tasks for future dates or today."
  (interactive)
  (when (and (planner-derived-mode-p 'planner-mode)
             (planner-page-name)
             (string-match planner-date-regexp (planner-page-name))
             (or (string< (planner-today) (planner-page-name))
                 (string= (planner-today) (planner-page-name))))
    (mapcar
     (lambda (task-string)
       (when task-string
         (planner-cyclic-create-task-maybe (planner-page-name)
                                           task-string)))
     (planner-cyclic-get-cyclic-tasks (planner-page-name)))))

(add-hook 'planner-mode-hook 'planner-cyclic-create-tasks-maybe)

(provide 'planner-cyclic)

;;; planner-cyclic.el ends here
