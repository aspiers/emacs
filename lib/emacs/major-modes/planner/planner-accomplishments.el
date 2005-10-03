;;; planner-accomplishments.el --- Accomplishment reports for planner.el

;; Copyright (C) 2004 Sandra Jean Chua (Sacha)

;; Emacs Lisp Archive Entry
;; Filename: planner-accomplishments.el
;; Version: 2005.08.20-17.59-stable
;; Keywords: hypermedia
;; Author: Sandra Jean Chua (Sacha) <sacha@free.net.ph>
;; Description: Produce accomplishment reports for planner.el
;; URL: http://sacha.free.net.ph/notebook/emacs/dev/planner/planner-accomplishments.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/dev/planner/ChangeLog
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

;;; Commentary:
;;
;; planner-accomplishments.el produces accomplishment reports for
;; planner files. On date pages, it summarizes tasks by associated PlanPage.
;;
;; DISPLAYING A TEMPORARY BUFFER
;;
;; You can call `planner-accomplishments-show' to display a buffer
;; containing the current page's accomplishment report.
;;
;; REWRITING SECTIONS OF YOUR PLANNER
;;
;; Choose this approach if you want accomplishment reports to be in
;; their own section and you would like them to be readable in your
;; plain text files even outside Emacs. Caveat: The accomplishment
;; section should already exist in your template and will be rewritten
;; when updated.
;;
;; To use, set `planner-accomplishments-section' to the name of the
;; section to rewrite (default: "Accomplishments"). If you want
;; rewriting to be automatically performed, call
;; `planner-accomplishments-insinuate'. The accomplishments will be
;; rewritten whenever you save a planner page. If you want rewriting
;; to be manual, call `planner-accomplishments-update'.
;;
;; TODO
;;
;; - On plan pages, it summarizes tasks by associated date page
;;   (controlled by `planner-accomplishments-plan-page-days'). Tasks
;;   are broken down by status.

(require 'planner)

;;; Code:

;;; USER VARIABLES -----------------------------------------------------------

(defgroup planner-accomplishments nil
  "Accomplishment reports for planner.el."
  :prefix "planner-accomplishments"
  :group 'planner)

(defcustom planner-accomplishments-section "Accomplishments"
  "Header for the accomplishments section in a plan page."
  :type 'string
  :group 'planner-accomplishments)

(defcustom planner-accomplishments-status-display
  '(("_" . "Unfinished")
    ("o" . "In progress")
    (">" . "Delegated")
    ("P" . "Postponed")
    ("X" . "Completed")
    ("C" . "Cancelled"))
  "Alist of status-label maps also defining the order of display."
  :type '(alist :key-type string :value-type string)
  :group 'planner-accomplishments)

(defvar planner-accomplishments-buffer "*Planner Accomplishments*"
  "Buffer name for accomplishment reports from `planner-accomplishments-show'.")

;;;###autoload
(defun planner-accomplishments-insinuate ()
  "Automatically call `planner-accomplishments-update'."
  (add-hook 'planner-mode-hook
            (lambda ()
              (add-hook
               (cond
                ((boundp 'write-file-functions) 'write-file-functions)
                ((boundp 'write-file-hooks) 'write-file-hooks))
               'planner-accomplishments-update nil t))))

;;;###autoload
(defun planner-accomplishments-update ()
  "Update `planner-accomplishments-section'."
  (interactive)
  (save-excursion
    (save-restriction
      (when (planner-narrow-to-section planner-accomplishments-section)
        (delete-region (point-min) (point-max))
        (insert "* " planner-accomplishments-section "\n\n"
                (planner-accomplishments-format-table
                 (planner-accomplishments-extract-data))
                "\n")
        nil)))) ; Return nil for write-file-functions

;;;###autoload
(defun planner-accomplishments-show ()
  "Display a buffer with the current page's accomplishment report."
  (interactive)
  (let ((page (and (planner-derived-mode-p 'planner-mode)
                   (planner-page-name)))
        (data (planner-accomplishments-extract-data)))
    (when page
      (with-current-buffer (get-buffer-create planner-accomplishments-buffer)
        (erase-buffer)
        (let ((emacs-wiki-project planner-project))
          (insert "Accomplishment report for " (planner-make-link page) "\n\n"
                  (planner-accomplishments-format-table data)
                  "\n")
          (planner-mode))
        (goto-char (point-min))
        (display-buffer (current-buffer) t)))))

(defun planner-accomplishments-extract-data ()
  "Return a list of ((link . status) . count) for tasks on the current page."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (results)
        (while (re-search-forward planner-task-regexp nil t)
          (let* ((info (planner-current-task-info))
                 (key (cons (planner-task-link info)
                            (planner-task-status info)))
                 (entry (assoc key results)))
            (if entry
                (setcdr entry (1+ (cdr entry)))
              (setq results (cons (cons key 1) results)))))
        results))))

(defun planner-accomplishments-total-by-link (data)
  "Return a list of (link . total)."
  (let (results)
    (mapcar
     (lambda (item)
       (let ((entry (assoc (car (car item)) results)))
         (if entry
             (setcdr entry (+ (cdr entry) (cdr item)))
           (setq results (cons (cons (car (car item)) (cdr item)) results)))))
     data)
    results))

(defun planner-accomplishments-total-by-status (data)
  "Return a list of (status . total)."
  (let (results)
    (mapcar
     (lambda (item)
       (let ((entry (assoc (cdr (car item)) results)))
         (if entry
             (setcdr entry (+ (cdr entry) (cdr item)))
           (setq results (cons (cons (cdr (car item)) (cdr item)) results)))))
     data)
    results))

(defun planner-accomplishments-format-table (data)
  "Format DATA from `planner-accomplishments-extract-data' into a table."
  (let ((links (planner-accomplishments-total-by-link data))
        (status (planner-accomplishments-total-by-status data))
        displayed-status)
    (setq links (sort links (lambda (a b) (> (cdr a) (cdr b)))))
    ;; Determine the status to be displayed
    (with-temp-buffer
      (insert "Link ")
      (mapcar
       (lambda (s)
         (when (assoc (car s) status)
           (insert " | " (cdr s))
           (setq displayed-status
                 (cons (assoc (car s) status) displayed-status))))
       planner-accomplishments-status-display)
      (insert " | Total\n")
      (setq displayed-status (nreverse displayed-status))
      (mapcar
       (lambda (page)
         (insert (if (car page)
                     (planner-make-link (car page))
                   "nil"))
         (mapcar
          (lambda (s)
            (insert
             (format " | %d"
                     (or (cdr (assoc (cons (car page) (car s)) data)) 0))))
          displayed-status)
         (insert " | " (number-to-string (cdr page)) "\n"))
       links)
      (insert "Total ")
      (let ((count 0))
        (mapcar
         (lambda (s)
           (setq count (+ count (cdr s)))
           (insert
            (format " | %d" (cdr s))))
         displayed-status)
        (insert (format " | %d\n" count)))
      (when (fboundp 'align-regexp)
        (align-regexp (point-min) (point-max) "\\(\\s-*\\)|" 1 1 t))
      (buffer-string))))

(provide 'planner-accomplishments)

;;; planner-accomplishments.el ends here
