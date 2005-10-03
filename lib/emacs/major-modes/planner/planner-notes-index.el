;;; planner-notes-index.el --- Note indexing support for the Emacs planner

;;; Commentary:

;;;_+ Package description

;; Copyright (C) 2004 Sandra Jean Chua <sacha@free.net.ph>
;; Filename: planner-notes-index.el
;; Version: 2005.08.20-17.59-stable
;; URL: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/
;; URL: http://sacha.free.net.ph/notebook/wiki/PlannerMode.php

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
;;
;;;_+ Usage
;;
;; Place planner-notes-index.el in your load path and add this to your
;; .emacs:
;;
;;    (require 'planner-notes-index)
;;
;; Then you can use tags of the form
;;
;;    <planner-notes-index page="2004.03.02">
;;    <planner-notes-index from="2004.03.01" to="2004.03.31">
;;    <planner-notes-index limit="10">
;;    <planner-notes-index page="PlanPage">
;;    <planner-notes-index-month-table month="2004.03" limit="5">
;;    <planner-notes-index-month-table month="2004.03">
;;
;; You can also use the following interactive functions:
;;
;; planner-notes-index
;; planner-notes-index-days
;; planner-notes-index-weeks
;; planner-notes-index-months
;; planner-notes-index-years    (wow!)
;;
;; These work based on the current date (date of current buffer, or today).
;;
;; If a single page is specified, this page is scanned for headlines
;; of the form:
;;
;; .#1 Headline
;;
;; and the results are presented as a bulleted list.
;;
;; If FROM and TO are specified, all date pages between them (inclusive)
;; are scanned. If FROM is omitted, it is assumed to be the earliest entry.
;; If TO is omitted, it is assumed to be the latest entry.
;;
;; If RECENT is specified, the list includes only that many recent headlines.
;;
;; and the results are presented as a bulleted list.
;;
;; To customize presentation, you can write a function that generates
;; the appropriate <planner-notes-index> tags. You can also use
;; planner-extract-note-headlines in your own functions.

(require 'planner)
(require 'calendar)

;;; Code:

;;;_* Internal functions

;; Old names, but then planner-notes-get-headlines was created.
;; These may disappear.
(defalias 'planner-notes-index-get-headlines
  'planner-notes-index-headlines-on-page)
(defalias 'planner-notes-index-get-headlines-range
  'planner-notes-index-headlines-in-range)

(defun planner-notes-index-headlines-on-page (page &optional limit)
  "Return a list of headlines in PAGE.
If LIMIT is non-nil, only that many headlines are returned."
  (when (stringp limit) (setq limit (string-to-number limit)))
  (with-temp-buffer
    (insert-file-contents (planner-page-file page))
    (planner-notes-get-headlines limit)))

(defun planner-notes-index-headlines-in-range (&optional from to limit)
  "Return a list of headlines over a span of day pages.
FROM (inclusive) and TO (inclusive) limit the dates. If FROM is
nil, start from the earliest entry. If TO is nil, include the
last entry. If LIMIT is non-nil, return at most LIMIT entries."
  (with-planner
    (when (and limit (stringp limit)) (setq limit (string-to-number limit)))
    (let ((pages (planner-get-day-pages from to))
          data
          headlines)
      (while (and pages (if limit (> limit 0) t))
        (setq data (planner-notes-index-headlines-on-page (car pages) limit))
        (when limit
          (setq limit (- limit (length data))))
        (when data
          (add-to-list 'headlines
                       (cons (car pages) data) t))
        (setq pages (cdr pages)))
      headlines)))

(defun planner-notes-index-insert-as-list (page-headlines &optional
  limit prefix suffix)
  "Link and format PAGE-HEADLINES.
PAGE-HEADLINES is a list of the form (page ((anchor headline) ...).
If LIMIT is non-nil, only display that number of recent items.
If PREFIX is non-nil, it is prepended to the item.
If SUFFIX is non-nil, it is appended to the item."
  (when (and limit (stringp limit)) (setq limit (string-to-number limit)))
  (let ((page (car page-headlines)))
    (setq page-headlines (cdr page-headlines))
    (while (and page-headlines (if limit (> limit 0) t))
      (when prefix (insert prefix))
      (insert (planner-make-link (concat page (caar page-headlines))
				 (cdr (car page-headlines))
				 t))
      (when suffix (insert suffix))
      (setq page-headlines (cdr page-headlines))
      (when limit (setq limit (1- limit))))))

;;;_* Emacs-wiki tags

;;;###autoload
(defun planner-notes-index-tag (tag-beg tag-end attrs)
  "Mark up planner-notes-index tags.

Tags can be of the form:

  <planner-notes-index page=\"2004.03.02\">
  <planner-notes-index from=\"2004.03.01\" to=\"2004.03.31\">
  <planner-notes-index limit=\"10\">"
  (let (last-month last-year)
    (mapc
     (lambda (item)
       (when (string-match planner-date-regexp (car item))
         (unless (and last-year
                      (string= (match-string 1 (car item)) last-year))
           (insert "* "
                   (setq last-year (match-string 1 (car item)))
                   "\n\n"))
         (unless (and last-month
                      (string= (match-string 2 (car item)) last-month))
           (insert "** " last-year "."
                   (setq last-month (match-string 2 (car item))) "\n\n")))
       (insert "*** " (car item) "\n\n")
       (planner-notes-index-insert-as-list item nil "- " "\n")
       (insert "\n"))
     (if (assoc "page" attrs)
         (cons (cdr (assoc "page" attrs))
               (planner-notes-index-headlines-on-page
                (cdr (assoc "page" attrs))
                (cdr (assoc "limit" attrs))))
       (planner-notes-index-headlines-in-range
        (cdr (assoc "from" attrs))
        (cdr (assoc "to" attrs))
        (cdr (assoc "limit" attrs)))))))

;;;###autoload
(defun planner-notes-index-month-table-tag (beg end attrs)
  "Mark up a month note index.  Tag is from BEG to END.
ATTRS is a list of attributes. \"Month\" is a yyyy.mm
 string (default: current month). \"Limit\" is the maximum number
 of items per day (default: all).

Examples:
<planner-notes-index-month-table month=\"2004.02\">
<planner-notes-index-month-table month=\"2004.02\" limit=\"4\">"
  (let ((month (cdr (assoc "month" attrs)))
        (limit (cdr (assoc "limit" attrs)))
        day headlines last dow)
    (unless month
      (setq month (substring (planner-get-current-date-filename) 0 7)))
    (when limit (setq limit (string-to-number limit)))
    (setq last (planner-filename-to-calendar-date (concat month ".31")))
    (setq last (calendar-last-day-of-month (elt last 0) (elt last 2)))
    ;; Chronologically sorted, at least by day
    (setq headlines
          (nreverse
           (planner-notes-index-headlines-in-range
            (concat month ".01") (concat month ".31")))) ;; works
    ;; Offset to negative if month does not start on Sunday
    (setq day (- 1 (calendar-day-of-week
                    (planner-filename-to-calendar-date (concat month ".01")))))
    (setq dow 0)
    (insert
     "<table class=\"month_notes\">"
     "<tr><th>Sun</th><th>Mon</th><th>Tue</th><th>Wed</th>"
     "<th>Thu</th><th>Fri</th><th>Sat</th></tr>")
    (while (or (< day 31) (< day 6))
      (when (= dow 0) (insert "<tr>"))
      (insert
       "<td><div class=\"month_day\">"
       (if (and (>= day 1) (<= day last))
           (format "[[%s.%02d]]" month day)
         "")
       "</div><div class=\"month_details\">\n")
      (let ((data (assoc (format "%s.%02d" month day)
                         headlines))
            extra)
        (planner-notes-index-insert-as-list
         data limit nil "<br>")

        (when (and limit data (> (length data) limit))
          (setq extra (- (length data) limit))
          (insert (format "%d more entr%s" extra
                          (if (= extra 1) "y" "ies")))))
      (insert "</div></td>")
      (when (= dow 6)
        (insert "</tr>"))
      (setq day (1+ day))
      (setq dow (% (1+ dow) 7)))
    (insert "</table>")))

;;;_* Other user functions

(defvar planner-notes-index-buffer "*Notes Index*"
  "Buffer for planner note index.")

;;;###autoload
(defun planner-notes-index (&optional from to limit)
  "Display a clickable notes index.
If called from a Lisp program, display only dates between FROM
and TO. With a prefix arg LIMIT, display only that number of
entries."
  (interactive "i
i
P")
  (when limit (setq limit (prefix-numeric-value limit)))
  (let ((headlines
         (planner-notes-index-headlines-in-range from to limit))
        last-month last-year)
    (with-current-buffer (get-buffer-create planner-notes-index-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (cd (planner-directory))
      (mapcar (lambda (item)
                (when (string-match planner-date-regexp (car item))
                  (unless (and last-year
                               (string= (match-string 1 (car item)) last-year))
                    (insert "* "
                            (setq last-year (match-string 1 (car item)))
                            "\n\n"))
                  (unless (and last-month
                               (string= (match-string 2 (car item)) last-month))
                  (insert "** " last-year "."
                          (setq last-month
                                (match-string 2 (car item))) "\n\n")))
                (insert "*** " (car item) "\n\n")
                (planner-notes-index-insert-as-list item nil "- " "\n")
                (insert "\n"))
              headlines)
      (let ((emacs-wiki-current-project planner-project))
        (planner-mode)
        (emacs-wiki-change-project planner-project))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun planner-notes-index-days (days)
  "Display an index of notes posted over the past few DAYS.
The list ends with the day of the current buffer or `planner-today'."
  (interactive (list (read-string "Number of days (1): " nil nil "1")))
  (when (stringp days) (setq days (string-to-number days)))
  (planner-notes-index
   (planner-calculate-date-from-day-offset
    (planner-get-current-date-filename) (- 1 days))
   (planner-get-current-date-filename)))

;;;###autoload
(defun planner-notes-index-weeks (weeks)
  "Display an index of notes posted over the past few WEEKS.
The list ends with the week of the current buffer or `planner-today'.
Weeks start from Sunday."
  (interactive (list (read-string "Number of weeks (1): " nil nil "1")))
  (when (stringp weeks) (setq weeks (string-to-number weeks)))
  (let ((date (planner-filename-to-calendar-date
               (planner-get-current-date-filename))))
    (planner-notes-index
     (planner-date-to-filename
      (encode-time 0 0 0 (- (elt date 1)
                            (calendar-day-of-week date)
                            (* (- weeks 1) 7))
                   (elt date 0) (elt date 2)))
     (planner-get-current-date-filename))))

;;;###autoload
(defun planner-notes-index-months (months)
  "Display an index of notes posted over the past few MONTHS.
The list ends with the month of the current buffer or `planner-today'."
  (interactive (list (read-string "Number of months (1): " nil nil "1")))
  (when (stringp months) (setq months (string-to-number months)))
  (let ((date (planner-filename-to-calendar-date
               (planner-get-current-date-filename))))
    (planner-notes-index
     (planner-date-to-filename
      (encode-time 0 0 0 1
                   (- (elt date 0)
                      months -1) (elt date 2)))
     (planner-get-current-date-filename))))

;;;###autoload
(defun planner-notes-index-years (years)
  "Display an index of notes posted over the past few YEARS.
The current year is included."
  (interactive (list (read-string "Number of years (1): " nil nil "1")))
  (when (stringp years) (setq years (string-to-number years)))
  (let ((date (planner-filename-to-calendar-date (planner-today))))
    (planner-notes-index
     (planner-date-to-filename
      (encode-time 0 0 0 1
                   1
                   (- (elt date 2)
                      years -1)))
     (planner-get-current-date-filename))))

;;;_* Initialization

(add-to-list 'planner-markup-tags
             '("planner-notes-index" nil t nil
               planner-notes-index-tag))
(add-to-list 'planner-markup-tags
             '("planner-notes-index-month-table" nil t nil
               planner-notes-index-month-table-tag))
(planner-update-wiki-project)

(provide 'planner-notes-index)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* : )
;; End:

;;; planner.el ends here
