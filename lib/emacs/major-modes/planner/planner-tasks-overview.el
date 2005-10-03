;;; planner-tasks-overview.el --- Task summary for planner.el
;;
;; Copyright (C) 2004 Sandra Jean Chua (Sacha) <sacha@free.net.ph>

;; Emacs Lisp Archive Entry
;; Filename: planner-tasks-overview.el
;; Version: 2005.08.20-17.59-stable
;; Keywords: hypermedia
;; Author: Sandra Jean Chua (Sacha) <sacha@free.net.ph>
;; Description: Task overview for planner.el files
;; URL: http://sacha.free.net.ph/notebook/emacs/planner/planner-tasks-overview.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/planner/ChangeLog
;; Compatibility: Emacs20, Emacs21

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

;;;_ + Commentary:

;; You can use `planner-tasks-overview' to see a list of tasks between
;; two dates. `planner-tasks-overview-jump' jumps to the linked task.
;; To change the sort order, invoke the following functions:
;; `planner-tasks-overview-sort-by-date'
;; `planner-tasks-overview-sort-by-plan'
;; `planner-tasks-overview-sort-by-category'
;; `planner-tasks-overview-sort-by-status'
;;
;; This file was inspired by Markus Hoenicka's
;; http://www.mhoenicka.de/software/hacks/tasklist.html and Frederick
;; Fouvry's Lisp port of tasklist.pl.

;;;_ + Contributors

;; Andrew J. Korty (ajk AT iu DOT edu) provided a patch that corrected
;; a typo in the keymap.

;;;_ + Thanks

;; A short, partial list of contributors, those who reported bugs, and
;; those who gave valuable suggestions can be found at
;; http://sacha.free.net.ph/notebook/wiki/PlannerMode.php


;;; Code:

(defvar planner-tasks-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" 'planner-tasks-overview-sort-by-date)
    (define-key map "2" 'planner-tasks-overview-sort-by-plan)
    (define-key map "3" 'planner-tasks-overview-sort-by-priority)
    (define-key map "4" 'planner-tasks-overview-sort-by-status)
    (define-key map "j" 'planner-tasks-overview-jump)
    (define-key map "o" 'planner-tasks-overview)

    (define-key map [tab] 'planner-next-reference)
    (define-key map [(control ?i)] 'planner-next-reference)
    (if (featurep 'xemacs)
        (define-key map [(shift tab)] 'planner-previous-reference)
      (define-key map [(shift iso-lefttab)] 'planner-previous-reference)
      (define-key map [(shift control ?i)] 'planner-previous-reference))
    map)
  "Keymap for planner task overview buffers.")

(define-derived-mode planner-tasks-overview-mode fundamental-mode "Overview"
  "Planner tasks overview.
\\{planner-tasks-overview-mode-map}")

(defvar planner-tasks-overview-data nil "Task data.")
(defvar planner-tasks-overview-buffer "*planner tasks overview*" "Buffer name.")

;;;###autoload
(defun planner-tasks-overview (start end)
  "Display a task overview from START to END."
  (interactive (list (planner-read-date)
                     (planner-read-date)))
  (when (get-buffer planner-tasks-overview-buffer)
    (kill-buffer planner-tasks-overview-buffer))
  (with-current-buffer (get-buffer-create planner-tasks-overview-buffer)
    (planner-tasks-overview-mode)
    (make-variable-buffer-local 'planner-tasks-overview-data)
    (setq planner-tasks-overview-data
          (planner-tasks-overview-extract-all-tasks
           (planner-get-day-pages start end)))
    (setq truncate-lines t)
    (set (make-variable-buffer-local 'truncate-partial-width-windows) t)
    (emacs-wiki-change-project planner-project)
    (planner-tasks-overview-sort-by-date)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun planner-tasks-overview-jump-other-window ()
  "Display the task under point in a different window."
  (interactive)
  (planner-tasks-overview-jump t))

;;;###autoload
(defun planner-tasks-overview-jump (&optional other-window)
  "Display the task under point."
  (interactive "P")
  (when other-window
    (switch-to-buffer-other-window (current-buffer)))
  (let* ((info (get-text-property (point) 'info))
         (page (or (elt info 2) (elt info 3))))
    (planner-find-file page)
    (goto-char (point-min))
    (widen)
    (when (re-search-forward
           (concat
            "#[A-C][0-9]*\\s-+.\\s-+"
            (regexp-quote (elt info 4))) nil t)
      (goto-char (planner-line-beginning-position)))))

(defun planner-tasks-overview-sort-by-field (field)
  "Sort by FIELD."
  (interactive)
  (setq planner-tasks-overview-data
        (sort planner-tasks-overview-data
              (lambda (a b) (string< (elt a field) (elt b field)))))
  (planner-tasks-overview-insert))

(defun planner-tasks-overview-sort-by-date ()
  "Sort by date."
  (interactive)
  (planner-tasks-overview-sort-by-field 2))

(defun planner-tasks-overview-sort-by-plan ()
  "Sort by plan."
  (interactive)
  (planner-tasks-overview-sort-by-field 3))

(defun planner-tasks-overview-sort-by-priority ()
  "Sort by plan."
  (interactive)
  (planner-tasks-overview-sort-by-field 0))

(defun planner-tasks-overview-sort-by-status ()
  "Sort by status."
  (interactive)
  (setq planner-tasks-overview-data
        (sort planner-tasks-overview-data
              (lambda (a b)
                (if (string= (elt b 1)
                             (elt a 1))
                    (string< (elt a 0)
                             (elt b 0))
                  (member (elt b 1)
                          (member (elt a 1)
                                  '("_" "o" ">" "P" "X" "C")))))))
  (planner-tasks-overview-insert))

(defun planner-tasks-overview-insert ()
  "Insert the textual representation for `planner-tasks-overview-data'.
DATA is a list of (priority status date plan description)."
  (with-current-buffer (get-buffer-create "*planner tasks overview*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (let (last-date last-plan)
      (mapcar
       (lambda (item)
         (let ((text
                (format "%10s | %s | %s %s | %s\n"
                        (if (elt item 2)
                            (planner-make-link
                             (elt item 2)
                             (format "%-10.10s"
                                     (if (string= last-date (elt item 2))
                                         "__________"
                                       (elt item 2))))
                          (format "%-10.10s" "nil"))
                        (if (elt item 3)
                            (planner-make-link
                             (elt item 3)
                             (format "%-20.20s"
                                     (if (string= last-plan (elt item 3))
                                         "____________________"
                                       (elt item 3))))
                          (format "%-20.20s" "nil"))
                        (elt item 0)
                        (elt item 1)
                        (elt item 4))))
           (add-text-properties 0 (length text) (list 'info item)
                                text)
           (insert text))
         (setq last-date (elt item 2))
         (setq last-plan (elt item 3)))
       planner-tasks-overview-data)
      (planner-mode)
      (goto-char (point-min))
      (setq buffer-read-only t))))

;; (planner-tasks-overview-extract-all-tasks (planner-get-day-pages start end))
;; (planner-tasks-overview-extract-all-tasks (planner-get-day-pages (planner-today)))
(defun planner-tasks-overview-extract-all-tasks (file-list)
  "Return a list of all tasks on pages in FILE-LIST."
  (let (results)
    (with-temp-buffer
      (cd (planner-directory))
      ;; The following line greps only the days limited by START and END.
      (apply 'call-process "grep" nil t nil "-H" "-e" "^#[A-C][0-9]*"
             file-list)
;;; The following line searches all the date pages.
      ;; (shell-command "grep -H -e '^#[A-C][0-9]*' --include='[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]' *" t)
;;; The following line searches date _and_ plan pages.
      ;;      (shell-command "grep -H -e '^#[A-C][0-9]*' --exclude='#*' *" t)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\([^:]+\\):\\(.+\\)")
          (let ((info (planner-task-info-from-string (match-string 1) (match-string 2))))
            (add-to-list
             'results
             (vector (planner-task-priority info)
                     (planner-task-status info)
                     (planner-task-date info)
                     (planner-task-plan info)
                     (planner-task-description info)))))
        (forward-line))
      results)))

;; Improvements: sort?
;;;###autoload
(defun planner-tasks-overview-show-summary (&optional file-list)
  "Count unscheduled, scheduled, and completed tasks for FILE-LIST.
If called with an interactive prefix, prompt for the page(s) to
display. planner-multi is required for multiple pages."
  (interactive
   (list
    (if current-prefix-arg
        (planner-file-alist
         nil
         (if (featurep 'planner-multi)
             (mapcar 'emacs-wiki-wiki-base
                     (planner-multi-split
                      (planner-read-non-date-page
                       (planner-file-alist))))
           (list (planner-read-non-date-page
                  (planner-file-alist))))))))
  (let (data total)
    (with-planner
      (setq data (planner-tasks-overview-get-summary file-list))
      (with-current-buffer (get-buffer-create "*planner tasks overview*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq total (+ (elt data 0) (elt data 1) (elt data 2)))
        (insert (format "Total unfinished, unscheduled tasks : %3d (%6.2f%%)\n"
                        (elt data 0) (/ (elt data 0) (* 0.01 total)))
                (format "Total unfinished, scheduled tasks   : %3d (%6.2f%%)\n"
                        (elt data 1) (/ (elt data 1) (* 0.01 total)))
                (format "Total finished tasks                : %3d (%6.2f%%)\n\n"
                        (elt data 2) (/ (elt data 2) (* 0.01 total))))
        (insert (format "%-40s | Unsched  |  Sched   | Complete | Total\n"
                        "Plan page"))
        (mapcar
         (lambda (row)
           (let ((row-total (* 0.01 (+ (elt row 1) (elt row 2) (elt row 3)))))
             (insert
              (format "%s | %3d %3.0f%% | %3d %3.0f%% | %3d %3.0f%% | %3d %3.0f%%\n"
                      (planner-make-link
                       (elt row 0)
                       (format "%-40.40s" (elt row 0)))
                      (elt row 1)
                      (/ (elt row 1) row-total)
                      (elt row 2)
                      (/ (elt row 2) row-total)
                      (elt row 3)
                      (/ (elt row 3) row-total)
                      (+ (elt row 1) (elt row 2) (elt row 3))
                      (/ (+ (elt row 1) (elt row 2) (elt row 3))
                         (* 0.01 total))))))
         (elt data 3))
        (cd (planner-directory))
        (let ((emacs-wiki-project planner-project))
          (planner-mode))
        (switch-to-buffer (current-buffer))))))

;; Unfinished                Finished      % Complete
;; Unscheduled   Scheduled
(defun planner-tasks-overview-get-summary (&optional file-list)
  "Return a summary of tasks on pages in FILE-LIST.
List is of the form (total-unfinished-unscheduled
total-unfinished-scheduled total-finished data), where data is a
list of the form (plan unfinished-unscheduled
unfinished-scheduled finished)."
  (let ((total-unfinished-unscheduled 0)
        (total-unfinished-scheduled 0)
        (total-finished 0)
        list)
    (unless file-list (setq file-list (planner-file-alist)))
    (with-temp-buffer
      (with-planner
        (while file-list
          (unless (string-match planner-date-regexp (car (car file-list)))
            (let ((unfinished-unscheduled 0)
                  (unfinished-scheduled 0)
                  (finished 0))
              (erase-buffer)
              (insert-file-contents (cdr (car file-list)))
              (goto-char (point-min))
              (while (re-search-forward planner-task-regexp nil t)
                (let ((info (planner-task-info-from-string
                             (car (car file-list))
                             (buffer-substring (planner-line-beginning-position)
                                               (planner-line-end-position)))))
                  (cond
                   ((or (string= (planner-task-status info) "X")
                        (string= (planner-task-status info) "C"))
                    (setq finished (1+ finished)))
                   ((planner-task-date info)
                    (setq unfinished-scheduled (1+ unfinished-scheduled)))
                   (t (setq unfinished-unscheduled (1+ unfinished-unscheduled))))))
              (setq list (cons (list (car (car file-list))
                                     unfinished-unscheduled
                                     unfinished-scheduled
                                      finished)
                                list))
               (setq total-unfinished-unscheduled
                     (+ total-unfinished-unscheduled unfinished-unscheduled))
               (setq total-unfinished-scheduled
                     (+ total-unfinished-scheduled unfinished-scheduled))
               (setq total-finished
                     (+ total-finished finished))))
          (setq file-list (cdr file-list)))))
    (list total-unfinished-unscheduled
          total-unfinished-scheduled
          total-finished
          list)))

(provide 'planner-tasks-overview)

;;; planner-tasks-overview.el ends here
