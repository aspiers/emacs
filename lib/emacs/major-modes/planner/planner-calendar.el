;;; planner-calendar.el --- Create a clickable calendar in published html

;; Copyright 2003, 2004 Gary V. Vaughan (gary AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: planner-calendar.el
;; Version: 1.1
;; Date: Tue, 1 June 2004
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Create a clickable calendar in published html
;; URL: http://tkd.kicks-ass.net/dist/planner-calendar.el
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

;; I maintain the hypertext parts of my website with John Wiegley's
;; emacs-wiki.el, now maintained by Sacha Chua at
;; http://sacha.free.net/notebook/emacs/emacs-wiki.  You will need to
;; install a copy of that file before this one is of any use to you.

;; Read the documentation for `planner-calendar-insert-calendar-maybe',
;; `planner-calendar-move-calendar-to-top-of-page-maybe' and
;; `planner-calendar-create-today-link' for how to use the functions
;; in this file from emacs-wiki hooks.
;;
;; If you decide to create a today link for published planner pages,
;; add a hook function like this:
;;
;;   (add-hook 'emacs-wiki-mode-hook
;;             (lambda ()
;;               (add-hook 'emacs-wiki-after-file-publish-hook
;;                         'planner-calendar-create-today-link nil t)))


;;; Code:

(require 'calendar)
(require 'emacs-wiki)

(defgroup planner-calendar nil
  "Options controlling the behaviour of planner calendar publication."
  :group 'planner)

(defcustom planner-calendar-prev-month-button "&laquo;"
  "*Default html entity to use for previous month buttons."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-next-month-button "&raquo;"
  "*Default html entity to use for next month buttons."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-day-header-chars 3
  "*Default number of characters to use for day column header names."
  :type 'integer
  :group 'planner-calendar)

(defcustom planner-calendar-html-tag-marker "<div class=\"content\">"
  "*Default html block element to add calendar HTML to."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-today-page-name "today"
  "*Default base name for published today page link file."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-nop-buttons-flag t
  "Non-nil means add <nop> tags before navigation buttons in the calendar."
  :type 'boolean
  :group 'planner-calendar)

(defmacro planner-calendar-render (var begin end tag class &rest body)
  "Generate a row of days for the calendar."
  `(let (string)
     (calendar-for-loop ,var from ,begin to ,end do
      (let ((day (mod (+ calendar-week-start-day i) 7))
	    (wrap-p (and (= 6 (mod ,var 7)) (/= ,var ,end))))
	(setq string (concat string
			     "<" ,tag " class=\"" ,class " "
			     (calendar-day-name day nil t) "\">"
			     ,@body
			     "</" ,tag ">\n"
			     (and wrap-p "</tr>\n<tr>\n")))))
     string))

(put 'planner-calendar-render 'lisp-indent-function 1)

(defun planner-calendar-date-to-filename (date)
  "See `planner-date-to-filename' except don't choke on nil DATE."
  (and date (planner-date-to-filename date)))

;; calendar-week-start-day
(defun planner-calendar (month year &optional arrows)
  "Generate a string of html to render a clickable calendar for MONTH YEAR.
If ARROWS is non-nil, include prev/next month arrows."
  (let*
      ((blank-days			; at start of month
	(mod (- (calendar-day-of-week (list month 1 year))
		calendar-week-start-day)
	 7))
       (last (calendar-last-day-of-month month year))
       (pad-days			; at end of month
	(- 7 (1+ (calendar-day-of-week (list month last year)))))
       ;; don't use leading whitespace in the generated html, or the
       ;; other markup rules will add <blockquote> sections!
       (string
	(concat
	 "<table class=\"month-calendar\">\n"
	 "<tr class=\"month-calendar-head\">\n"
	 (if arrows
	     (concat
	      "<th>"
	      (planner-calendar-prev-month-href
	       month year
               planner-calendar-prev-month-button
               planner-calendar-nop-buttons-flag)
	      "</th>\n"
	      "<th colspan=\"5\">\n")
	   "<th colspan=\"7\">\n")
	 (format "%s %d" (calendar-month-name month) year)
	 "</th>\n"
	 (when arrows
	   (concat "<th>"
		   (planner-calendar-next-month-href
		    month year planner-calendar-next-month-button
                    planner-calendar-nop-buttons-flag)
		   "</th>\n"))
	 "</tr>\n"
	 "<tr>\n"

	 ;; add day name headings
	 (planner-calendar-render i 0 6
	  "th" "month-calendar-day-head"
	  (calendar-day-name day planner-calendar-day-header-chars t))

	 "</tr>\n"
	 "<tbody>\n"
	 "<tr>\n"

	 ;; add blank days before the first of the month
	 (planner-calendar-render i 0 (1- blank-days)
	  "td" "month-calendar-day-noday" "&nbsp;")

	 ;; put in the days of the month
	 (planner-calendar-render i blank-days (+ last blank-days -1)
	     "td" (if (planner-page-file
		       (planner-calendar-date-to-filename
			(list month (- i blank-days -1) year)))
		      "month-calendar-day-link"
		    "month-calendar-day-nolink")
	     (planner-calendar-published-file-href
		      (planner-calendar-date-to-filename
		       (list month (- i blank-days -1) year))
		      (int-to-string (- i blank-days -1))
		      planner-calendar-nop-buttons-flag))

	 ;; add padding days at end of month to make rule lines neat
	 (unless (zerop (mod (+ blank-days last) 7))
	   (planner-calendar-render i
	     (+ last blank-days) (+ last blank-days pad-days -1)
	     "td" "month-calendar-day-noday" "&nbsp;"))

	 "</tr>\n"
	 "</tbody>\n"
	 "</table>\n")))
    string))

(defun planner-calendar-from-wiki (&optional arrows wiki)
  "Generate a string of html (possibly with ARROWS) for a calendar for WIKI."
  (let ((page (or wiki (planner-page-name))))
    (save-match-data
      (when (string-match planner-date-regexp page)
	(let ((year (string-to-number (substring page 0 4)))
	      (month (string-to-number (substring page 5 7))))
	  (planner-calendar month year arrows))))))

(defun planner-calendar-published-file-href (wiki &optional name nop)
  "Return an href anchor string to the published WIKI if WIKI exists."
  (let ((string
	 (if (and (planner-page-file wiki)
		  (not (planner-private-p wiki)))
	     (emacs-wiki-link-href wiki (or name wiki))
	   (or name wiki))))
    (if (and nop wiki (string-match emacs-wiki-name-regexp wiki))
	(planner-replace-regexp-in-string (regexp-quote wiki) "<nop>\\&"
					  string)
      string)))

(defun planner-calendar-yesterday (date)
  "Return the day before DATE as a (month day year) list."
  (let* ((year (extract-calendar-year date))
	 (month (extract-calendar-month date))
	 (day (extract-calendar-day date))
	 (prev-year (if (and (= 1 month) (= 1 day)) (1- year) year))
	 (prev-month (if (= 1 day) (1+ (mod (+ month 10) 12)) month))
	 (prev-day (if (= 1 day)
		       (calendar-last-day-of-month prev-month prev-year)
		     (1- day))))
    (list prev-month prev-day prev-year)))

(defun planner-calendar-tomorrow (date)
  "Return the day after DATE as a (month day year) list."
  (let* ((year (extract-calendar-year date))
	 (month (extract-calendar-month date))
	 (day (extract-calendar-day date))
	 (last-day (calendar-last-day-of-month month year))
	 (next-year
	  (if (and (= 12 month) (= 31 day))
	      (1+ year)
	    year))
	 (next-month
	  (if (>= day last-day)
	      (1+ (mod month 12))
	    month))
	 (next-day (if (< day last-day) (1+ day) 1)))
    (list next-month next-day next-year)))

(defun planner-calendar-today (&optional max-days)
  "Return today or the first day before today with a day page."
  (planner-calendar-prev-date
   (planner-calendar-tomorrow (calendar-current-date))))

(defun planner-calendar-create-today-link (&optional name)
  "Create a link to the newest published day page.
Add this to `emacs-wiki-after-publish-hook' to create a \"today\" soft
link to the newest published planner day page, on operating systems that
support POSIX \"ln\"."
  (let* ((today-name planner-calendar-today-page-name)
	 (target-file (planner-published-file (or name today-name)))
	 (source-file (planner-published-file
		       (planner-calendar-date-to-filename
			(planner-calendar-today)))))
    (when (file-exists-p target-file)
      (funcall planner-delete-file-function target-file))
    (make-symbolic-link source-file target-file t)))

(defun planner-calendar-prev-date (date &optional max-days)
  "Return the first day before DATE with a day page."
  (let ((days (or max-days 180))
	(yesterday date)
	(done nil))
    (while (and (not done) (> days 0))
      (setq yesterday (planner-calendar-yesterday yesterday)
	    days (1- days))
      (let ((wiki (planner-calendar-date-to-filename yesterday)))
	(setq done (and (planner-page-file wiki)
			(not (planner-private-p wiki))))))
    (if done yesterday nil)))

(defun planner-calendar-next-date (date &optional max-days)
  "Return the first day after DATE with a day page."
  (let ((days (or max-days 180))
	(tomorrow date)
	(done nil))
    (while (and (not done) (> days 0))
      (setq tomorrow (planner-calendar-tomorrow tomorrow)
	    days (1- days))
      (let ((wiki (planner-calendar-date-to-filename tomorrow)))
	(setq done (and (planner-page-file wiki)
			(not (planner-private-p wiki))))))
    (if done tomorrow nil)))

(defun planner-calendar-prev-date-href (date name &optional nop max-days)
  "Return an href anchor string for the first day page before DATE."
  (let ((prev-date (planner-calendar-prev-date date max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename prev-date) name nop)))

(defun planner-calendar-next-date-href (date name &optional nop max-days)
  "Return an href anchor string for the first day page after DATE."
  (let ((next-date (planner-calendar-next-date date max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename next-date) name nop)))

(defun planner-calendar-prev-month-href (month year name &optional nop max-days)
  "Return an href anchor string for the last day page in the previous month."
  (let ((prev-date (planner-calendar-prev-date (list month 1 year) max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename prev-date) name nop)))

(defun planner-calendar-next-month-href (month year name &optional nop max-days)
  "Return an href anchor string for the first day page in the following month."
  (let ((next-date
	 (planner-calendar-next-date
	  (list month (calendar-last-day-of-month month year) year)
	  max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename next-date) name nop)))

(defun planner-calendar-prev-day-wiki (&optional wiki max-days)
  "Return the first planner day page before this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-date-to-filename (planner-calendar-prev-date date max-days))))

(defun planner-calendar-next-day-wiki (&optional wiki max-days)
  "Return the first planner day page after this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-date-to-filename (planner-calendar-next-date date max-days))))

(defun planner-calendar-prev-date-href-from-wiki (name &optional wiki max-days)
  "Return an href anchor string for the first day page before this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-prev-date-href date name max-days)))

(defun planner-calendar-next-date-href-from-wiki (name &optional wiki max-days)
  "Return an href anchor string for the first day page after this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-next-date-href date name max-days)))

(defun planner-calendar-prev-month-href-from-wiki (name &optional wiki max-days)
  "Return a string for the last day page in first month before this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-prev-month-href date name max-days)))

(defun planner-calendar-next-month-href-from-wiki (name &optional wiki max-days)
  "Return a string for the first day page in the first month after this one."
  (let* ((page (or wiki (planner-page-name)))
	 (date (planner-filename-to-calendar-date page)))
    (planner-calendar-next-month-href date name max-days)))

(defun planner-calendar-insert-calendar-maybe ()
  "Insert the calendar on day pages.
Add this to `emacs-wiki-before-markup-hook'. This can't be done
from the page header, as header text is added after much of the
page buffer has been marked up."
  (let ((page (planner-page-name)))
    (when (and page (string-match planner-date-regexp page))
      (goto-char (point-min))
      (insert "<lisp>(planner-calendar-from-wiki 'arrows)</lisp>\n"))))

(defun planner-calendar-move-calendar-to-top-of-page-maybe ()
  "Move calendar to just after `planner-calendar-html-tag-marker'.
The calendar can't be inserted as HTML in
`emacs-wiki-after-markup-hook' as the some of the functions used
by `planner-calendar' only work during markup. So we insert the
unmarked-up calendar with `emacs-wiki-before-markup-hook' and
then move the generated HTML to the correct part of the page with
`emacs-wiki-after-markup-hook'."
  (when (string-match planner-date-regexp (or (planner-page-name) ""))
    (goto-char (point-min))
    (let* ((body (and (search-forward planner-calendar-html-tag-marker nil 'noerror)
		      (forward-line 1)
		      (point)))
	   (start (save-excursion
		    (and (search-forward "<table class=\"month-calendar\"")
			 (forward-line 0)
			 (point))))
	   (end (save-excursion
		  (and start
		       (goto-char start)
		       (search-forward "</table>")
		       (point))))
	   (calendar (and start end
			(buffer-substring start end))))
    (when (and body calendar)
      (delete-region start end)
      (goto-char body)
      (insert calendar (string-match planner-date-regexp (planner-page-name)))))))

(provide 'planner-calendar)

;;; planner-calendar.el ends here
