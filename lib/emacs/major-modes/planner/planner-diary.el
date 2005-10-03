;;; planner-diary.el --- Diary integration for the Emacs Planner (planner.el)

;; Copyright (C) 2003, 2004 Thomas Gehrlein <Thomas.Gehrlein AT t-online.de>
;; Parts copyright (C) 2004 Travis B. Hartwell <nafai AT travishartwell.net>

;; Emacs Lisp Archive Entry
;; Filename: planner-diary.el
;; Time-stamp: "2004-03-27 16:51:20 Thomas Gehrlein"
;; Version: 1.0-devel
;; Keywords: hypermedia
;; Author: Thomas Gehrlein <Thomas.Gehrlein@t-online.de>
;; Maintainer: Thomas Gehrlein <Thomas.Gehrlein@t-online.de>
;; Description: Integrate the Emacs Planner with Calendar and Diary
;; URL: http://sacha.free.net.ph/notebook/emacs/dev/planner/planner-diary.el
;; ChangeLog: Can be requested from the maintainer
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

;;; PLANNER-DIARY

;; (The following documentation describes the stable version of planner-diary
;; (v1.0).  If planner-diary doesn't do what I claim here, then it's a bug and you
;; will fix it.  This file contains additional code that is part of the
;; development version of planner.  This code may or may not work.  Use it at your
;; own risk.)
;;
;;; Commentary:
;; 
;; If you use Emacs' diary feature, planner-diary could be helpful for you.  It
;; puts all diary entries for the current day in the "* Diary" section of your day
;; plan page.  This section is updated every time you display the file in Emacs.
;; By default the diary section of past pages is not updated (it's pretty unlikely
;; that you want to add new diary entries for the past).
;;
;; If you want to use planner-diary.el, put the file in your load path and add
;; this to your .emacs:
;;
;; (require 'planner-diary)
;;
;; (This step should not be necessary once a stable planner package will be put
;; together.)
;;
;; planner-diary.el needs fancy-diary-display.  To use fancy-diary-display add
;; this to your .emacs:
;;
;; (add-hook 'diary-display-hook 'fancy-diary-display)
;;
;; You can use planner diary in two different ways:
;;
;; 1) If you want the saved files to contain your entries and not just a line
;;    of lisp, add the following lines to your .emacs:
;;
;;    (setq planner-diary-use-diary t)
;;    (planner-diary-insinuate)
;;
;;    You should also customize or set planner-day-page-template to include a
;;    "* Diary":
;;
;;    (setq planner-day-page-template
;;          "* Tasks\n\n\n* Schedule\n\n\n* Diary\n\n\n* Notes")
;;
;;    C-c C-e updates the diary sections.  C-u C-c C-e forces an update, it
;;    inserts the diary section for the day, even if the day is in the past or
;;    if there is no Diary section in the buffer.
;;
;; 2) (GNU EMACS ONLY) You can put the following line of lisp code in
;;    your day plan pages to display your diary entries:
;;
;;    <lisp>(planner-diary-entries-here)</lisp>
;;
;;    You can do this automatically for all day plan pages:
;;
;;    (setq planner-day-page-template
;;          "* Tasks\n\n\n* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>\n\n* Notes")
;;
;;    When you open a day plan page outside emacs, you will see the line of lisp
;;    code and not your diary entries.
;;
;; If you want to see your diary entries for more than just 1 day, set
;; `planner-diary-number-of-diary-entries' accordingly.  This works for
;; either of the 2 approaches.
;;
;; If you want to use the cal-desk package, simply follow the instructions in
;; cal-desk.el.  If you get the cal-desk layout from the Calendar buffer, you get
;; it in the day plan buffer, too.
;;
;; If you use planner-diary, you might consider using the Calendar support of planner.
;; To get Calendar integration add this to your .emacs:
;;
;;    (planner-insinuate-calendar)
;;
;; For further information refer to the documentation for this function.
;;
;; If you have questions about planner-diary, feature requests, bug reports or
;; anything else you want to tell me: thomas.gehrlein@t-online.de.

;;; THANKS TO:

;; - Jody Klymak (jklymak AT coas DOT oregonstate DOT edu)
;;   Added some documentation
;;
;; - Sacha Chua (sacha AT free DOT net DOT ph)
;;   Thoughts and ideas
;;
;; - Bastien Guerry
;;   Bug report and appointment integration


;;; HISTORY:

;; version 1.0 First stable version.  Expected to work as documented.  Same
;; feature of previous versions are not supported.
;;
;; version 0.7 Major rewrite.  New diary types (cal-desk, appointments, privat,
;; public diaries).  Add diary entries from planner pages.
::
;; version 0.6 minor changes
;;
;; version 0.5 appointment integration (thanks to Bastien Guerry)

;;; ADVANCED FEATURES

;; (The features described here are part of the development version.  They are
;; subject to change without notice.  They may be buggy.  The documentation may
;; be inaccurate.  Use at your own risk.)

;; There is a lot of code redundancy in the development version.  This is
;; intentional and makes it easier to change the code for one type of diary
;; section without breaking others.

;; Currently planner-diary supports 6 different views of your diary entries:

;; 1) Ordinary fancy diary display (what you get by pressing d in the calendar
;;    buffer with fancy-diary-display switched on)
;; 2) Schedule/Appointments (all entries from 1 that have a time assigned to
;;    them.
;; 3) Diary without appts (1 without 2).
;; 4) cal-desk display (appts on top, non appts entries at bottom)
;; 5) A private diary (same as 1, but uses a different diary-file)
;; 6) A public diary (same as 1, but uses a different diary-file)

;; 1) Put the following line of lisp code in you day plan pages to display your
;;    diary entries:
;; 
;;    <lisp>(planner-diary-entries-here)</lisp>
;;
;;    The function `planner-diary-entries-here' takes two optional args:  The
;;    diary file you want to use and the number of days you want to display.
;;

;;; RANDOM THOUGHTS ABOUT PLANNER, CALENDAR, AND DIARY

;; This is planner-diary, not planner-calendar!  Include only stuff that has to
;; do with diary and planner, leave the calendar stuff to
;; planner-insinuate-calendar.

;; Deal with holidays.

;; There are 3 ways to use planner with calendar and diary:

;; 1) Use calendar for browsing the day plan pages.

;; Cool features: all days with day plan pages are marked.  ">" and "<" move to
;; the next or previous page.  (Ordinary diary entries should not be marked.)
;; Entry point: a command (planner-browse-calendar ??)  What about key-bindings
;; for "n" and "N"?

;; 2) Display the day plan pages when you move in calendar.

;; Add a hook to calendar-move-hook.  Add key-bindings for "n" and "N".

;; 3) Automatically update the diary section in day plan pages.

;; Add a function to planner-goto-hook.  This work independent of the first 2.

;;;_ + Contributors

;; Travis B. Hartwell (nafai AT travishartwell DOT net) helped fix
;; calendar desk entries and fixed a typo or two.

(require 'planner)
(require 'diary-lib)
(require 'calendar)
(require 'cl)

;;; Code:
(defgroup planner-diary nil
  "Diary integration for planner.el."
  :prefix "planner-diary-"
  :group 'planner)

;;; USER VARIABLES
;; STANDARD DIARY SUPPORT (no fancy extras - this is what I, the author of
;: this file, use)
(defcustom planner-diary-string "* Diary"
  "*Header for the diary section in a day plan page."
  :type 'string
  :group 'planner-diary)

(defcustom planner-diary-create-section-flag t
  "Non-nil means create diary section in future or present day pages."
  :type 'boolean
  :group 'planner-diary)
  
(defcustom planner-diary-file diary-file
  ;; FIXME: Test if this works as buffer-local var
  "*The diary file you want to use with planner-diary."
  :type 'file
  :group 'planner-diary)

(defcustom planner-diary-use-diary nil
;;   (if (string-match planner-diary-string planner-day-page-template)
;;       t
;;     nil)
  "*Non-nil means: Automatically insert a diary section into day plan pages.
Uses the standard fancy-diary display."
  :type 'boolean
  :group 'planner-diary)

(defcustom planner-diary-number-of-days 1
  "*Number of days of diary entries displayed in the standard diary section."
  :type 'number
  :group 'planner-diary)

(defcustom planner-diary-include-all-output-flag nil
  "Non-nil means don't omit any data when copying diary entries into day pages."
  :type 'boolean
  :group 'planner-diary)

;; for backward compatability
;; (defcustom planner-diary-number-of-diary-entries 1
;;   "*Obsolete, use `planner-diary-number-of-days' instead."
;;   :type 'number
;;   :group 'planner-diary)


;; CAL-DESK
(defcustom planner-diary-cal-desk-string "* Cal-Desk"
  "Header for the cal-desk section in a day plan page.
You might want to use \"* Diary\" or \"* Schedule\"."
  :type 'string
  :group 'planner-diary)

(defcustom planner-diary-cal-desk-file planner-diary-file
  "The diary file you want to use for your cal-desk section."
  :type 'file
  :group 'planner-diary)

(defcustom planner-diary-use-cal-desk nil
;;   (if (string-match planner-diary-cal-desk-string planner-day-page-template)
;;       t
;;     nil)
  "Non-nil means include a cal-desk section in day pages.
This section contains only entries from `planner-diary-cal-desk-file'."
  :type 'boolean
  :group 'planner-diary)

(defcustom planner-diary-cal-desk-number-of-days
  1                                     ; 1 is good for cal-desk's format.
  "Number of days of diary entries displayed in the cal-desk diary section."
  :type 'number
  :group 'planner-diary)

;; DIARY APPOINTMENTS (idea and most of this code from Bastien Guerry)
(defcustom planner-diary-appts-string "* Diary Appointments"
  "*Header for the diary appointments section in a day plan page."
  :type 'string
  :group 'planner-diary)

(defcustom planner-diary-appts-file planner-diary-file
  "*The diary file you want to use for your diary appointments section."
  :type 'file
  :group 'planner-diary)

(defcustom planner-diary-use-appts nil
;;   (if (string-match planner-diary-appts-string planner-day-page-template)
;;       t
;;     nil)
  "*Non-nil means: Insert a diary appointments section into day plan pages.
This displays all diary entries `planner-diary-appts-file' with a time assigned
to them."
  :type 'boolean
  :group 'planner-diary)

(defcustom planner-diary-appts-number-of-days
  1                                     ; 1 is good for appts
  "Number of days of diary entries displayed in the appointments diary section."
  :type 'number
  :group 'planner-diary)

(defcustom planner-diary-exclude-appts-from-diary
  planner-diary-use-appts            ; if you use appts, you want to exclude
                                     ; them from the diary section
  "Non-nil means that appts are not shown in the diary section.
This is useful if you use a diary appointments section.  Diary entries with a
time assigned to them will then only be displayed in the diary appointments
section and not in the diary section."
  :type 'boolean
  :group 'planner-diary)

;; PRIVATE DIARY
(defcustom planner-diary-private-string "* Private Diary"
  "Header for the private diary section in a day plan page.
This is the section you don't want to publish.  planner-diary doesn't do
anything about the publishing, it's up to you to make sure this section doesn't
get published."
  :type 'string
  :group 'planner-diary)

(defcustom planner-diary-private-file planner-diary-file
  ;; FIXME: Test if this works as buffer-local var
  "The diary file you want to use with for your private diary."
  :type 'file
  :group 'planner-diary)

(defcustom planner-diary-use-private-diary nil
;;   (if (string-match planner-diary-private-string planner-day-page-template)
;;       t
;;     nil)
  "Non-nil means: Insert a private diary section into day plan pages.
This section contains only entries from `planner-diary-private-file'."
  :type 'boolean
  :group 'planner-diary)

(defcustom planner-diary-private-number-of-days
  planner-diary-number-of-days
  "Number of days of diary entries displayed in the private diary section."
  :type 'number
  :group 'planner-diary)

;; PUBLIC DIARY
(defcustom planner-diary-public-string "* Public Diary"
  "Header for the public diary section in a day plan page.
This is the section you want to publish.  Obviously, this only makes sense if
you have a Private Diary section, too.  planner-diary doesn't do anything about
the publishing, it's up to you to make sure this section gets published."
  :type 'string
  :group 'planner-diary)

(defcustom planner-diary-public-file diary-file
  "The diary file you want to use with for your public diary."
  :type 'file
  :group 'planner-diary)

(defcustom planner-diary-use-public-diary nil
;;   (if (string-match planner-diary-public-string planner-day-page-template)
;;       t
;;     nil)
  "Non-nil means: Insert a public diary section into day plan pages.
This section contains only entries from `planner-diary-public-file'."
  :type 'boolean
  :group 'planner-diary)

(defcustom planner-diary-public-number-of-days
  planner-diary-number-of-days
  "Number of days of diary entries displayed in the public diary section."
  :type 'number
  :group 'planner-diary)


;;; INTERNAL VARS
;; FIXME: Is this used for anything?
(defcustom planner-diary-exclude-regexp ""
  "Regexp for diary entries not displayed in the diary section.
Used by the schedule code."
  :type 'regexp
  :group 'planner-diary)

(defcustom planner-diary-time-regexp "[0-2][0-9]:[0-5][0-9]"
  ;; FIXME: Internationalize?  (AM and PM)
  "A regexp for time in a diary appt entry."
  :type 'regexp
  :group 'planner-diary)

;;; FUNCTIONS
;; GETTING THE RELEVANT ENTRIES
;; planner-diary-get-diary-entries is the main function,
;; planner-diary-get-appts-entries needs to be rewritten.
;; planner-diary-get-[public/private/cal-desk]-entries call
;; planner-diary-get-diary-entries.
(defun planner-diary-get-diary-entries (date &optional no-of-days file
                                             use-cal-desk)
  "Get the diary entries for DATE and the following NO-OF-DAYS days from FILE.
DATE is a list (month day year). NO-OF-DAYS defaults to
`planner-diary-number-of-days'. FILE defaults to
`planner-diary-file'. Optional argument USE-CAL-DESK means
display using a fancy desk calendar."
  (save-window-excursion
    (let* ((fancy-diary-buffer "temporary-fancy-diary-buffer")
           (entries)
           (font-lock-defaults nil)
           (font-lock-mode nil)
           (diary-display-hook
            (if use-cal-desk
                '(sort-diary-entries fancy-diary-display
                                     fancy-schedule-display-desk-calendar)
              '(sort-diary-entries fancy-diary-display)))
           (no-of-days (or no-of-days planner-diary-number-of-days))
           (diary-file (or file planner-diary-file)))
      (list-diary-entries date no-of-days)
      (switch-to-buffer fancy-diary-buffer)
      (let ((inhibit-read-only t))
        ;; return "No entries" if buffer is empty
        (setq entries
              (if (= (point-max) 1)
                  "No entries"
                (buffer-substring
                 (if (> no-of-days 1)   ; if more than 1 day, show everything
                     (progn
                       (while (re-search-forward "^=+$" nil t)
                         (replace-match
                          (make-string (length (match-string 0)) ?-)))
                       (point-min))
                   (if planner-diary-include-all-output-flag
                     ;; remove date and lots of =s if just for 1 day
                       (point-min)
                     (goto-char (point-min))
                     (search-forward-regexp "^=+$") ; one or more =
                     (1+ (point))))
                 ;; remove final newline
                 (progn
                   (goto-char (point-max))
                   (when (bolp) (backward-char 1))
                   (point)))))
        (kill-buffer fancy-diary-buffer)
        entries))))

(defun planner-diary-get-cal-desk-entries (date &optional no-of-days file)
  "Get the cal-desk style diary entries for DATE.
Consider the following NO-OF-DAYS days from FILE. DATE is a
list (month day year). NO-OF-DAYS defaults to
`planner-diary-cal-desk-number-of-days'. FILE defaults to
`planner-diary-cal-desk-file'."
  (planner-diary-get-diary-entries date
                                   (or no-of-days
                                       planner-diary-cal-desk-number-of-days)
                                   (or file
                                       planner-diary-cal-desk-file)
                                   t))


;;; TODO: Merge the following two functions into one:
(defun planner-diary-get-appts-entries (date &optional no-of-days file)
  ;; FIXME: use optional args
  "Call `planner-diary-get-entries' for appointments on DATE.
Arguments NO-OF-DAYS and FILE are ignored."
  (planner-diary-get-entries date 'appt))

(defun planner-diary-get-entries (date &optional type)
  "Get the appointment diary entries for DATE and the following days.
DATE is a list (month day year).  Optional arg TYPE ..." ; FIXME: doc for type
  (save-window-excursion
    (let* ((regexp planner-diary-exclude-regexp)
	   (no-of-days planner-diary-number-of-days)
	   (init-entries (flet ((message (&rest args) (ignore args)))
			   (list-diary-entries date (if type 1 no-of-days))))
	   (end-entries
	    (mapcar
	     '(lambda (seq)
		(let ((seq2 (if (and (eq type 'appt)
				     (string-match planner-diary-time-regexp (cadr seq)))
				(planner-diary-appt-to-planner (cadr seq))
			      (cadr seq))))
		  (unless ; first check no excluded regexp
		      (or (and (not (equal regexp ""))
			       (string-match regexp seq2))
					; then check appt not already there
			  (string-match (substring seq2 0 (1- (length seq2)))
					; bug in planner.el ? a space is added
					; from diary entries to .diary.planner
					(buffer-string))
			  (if (eq type 'appt)
			      (not (string-match planner-diary-time-regexp seq2))
			    (and (eq type nil)
				 planner-diary-exclude-appts-from-diary
				 (string-match planner-diary-time-regexp
					       (cadr seq)))))
		    seq2))) init-entries)))
      (mapconcat 'eval (delete-duplicates (remove nil end-entries)
					  :test 'equal) "\n"))))

(defun planner-diary-appt-to-planner (appt)
  "Convert APPT from diary format to planner format."
  (string-match "\\([0-9]+:[0-9]+[ap]?m?\\)[ ]+\\(.*\\)" appt)
  (let ((time (match-string 1 appt))
	(task (match-string 2 appt)))
    (concat time " | " task)))


(defun planner-diary-get-private-entries (date &optional no-of-days file)
  "Get private diary entries for DATE and the next NO-OF-DAYS from FILE.
DATE is a list (month day year). NO-OF-DAYS defaults to
`planner-diary-private-number-of-days'. FILE defaults to
`planner-diary-private-file'."
  (planner-diary-get-diary-entries date
                                   (or no-of-days
                                       planner-diary-private-number-of-days)
                                   (or file
                                       planner-diary-private-file)))

(defun planner-diary-get-public-entries (date &optional no-of-days file)
  "Get public diary entries for DATE and the next NO-OF-DAYS days from FILE.
DATE is a list (month day year). NO-OF-DAYS defaults to
`planner-diary-public-number-of-days'. FILE defaults to
`planner-diary-public-file'."
  (planner-diary-get-diary-entries date
                                   (or no-of-days
                                       planner-diary-public-number-of-days)
                                   (or file
                                       planner-diary-public-file)))

(defun planner-diary-get-name ()
  "Return current filename."
  (planner-page-name))

;;; LISP FUNCTIONS FOR USE IN PLANNER DAY PAGES
;;; arg FILE to specify a diary-file (suggested by David O'Toole)
(defun planner-diary-entries-here (&optional file no-of-days)
  "Display the diary entries from FILE for the next NO-OF-DAYS days.
FILE defaults to `planner-diary-file', NO-OF-DAYS defaults to
`planner-diary-number-of-days'.

Put this is your day pages:
\"<lisp>(planner-diary-entries-here)</lisp>\"
or this, if you want to do fancy things:
\"<lisp>(planner-diary-entries-here \"/path/to/diary/file\" 1)</lisp>\"

You might want to use `planner-day-page-template' to do so."
  (planner-diary-get-diary-entries
   (planner-filename-to-calendar-date
    (planner-diary-get-name))
   (or no-of-days planner-diary-number-of-days)
   (or file planner-diary-file)))

(defun planner-diary-appts-entries-here (&optional file no-of-days)
  "Display the diary appointments entries from FILE for the next NO-OF-DAYS.
FILE defaults to `planner-diary-appts-file', NO-OF-DAYS defaults to
`planner-diary-appts-number-of-days'.

Put this is your day pages:
\"<lisp>(planner-diary-appts-entries-here)</lisp>\"
or this, if you want to do fancy things:
\"<lisp>(planner-diary-appts-entries-here \"/path/to/diary/file\" 1)</lisp>\"

You might want to use `planner-day-page-template' to do so."
  (planner-diary-get-appts-entries
   (planner-filename-to-calendar-date
    (planner-diary-get-name))
   (or file planner-diary-appts-file)
   (or no-of-days planner-diary-appts-number-of-days)))


(defun planner-diary-cal-desk-entries-here (&optional file no-of-days)
  "Display the diary appointments entries from FILE for the next NO-OF-DAYS.
FILE defaults to `planner-diary-cal-desk-file', NO-OF-DAYS defaults to
`planner-diary-cal-desk-number-of-days'.

Put this is your day pages:
\"<lisp>(planner-diary-cal-desk-entries-here)</lisp>\"
or this, if you want to do fancy things:
\"<lisp>(planner-diary-cal-desk-entries-here \"/path/to/diary/file\" 1)</lisp>\"

You might want to use `planner-day-page-template' to do so."
  (planner-diary-get-cal-desk-entries
   (planner-filename-to-calendar-date
    (planner-diary-get-name))
   (or file planner-diary-cal-desk-file)
   (or no-of-days planner-diary-cal-desk-number-of-days)))

(defun planner-diary-public-entries-here (&optional file no-of-days)
  "Display the diary appointments entries from FILE for the next NO-OF-DAYS.
FILE defaults to `planner-diary-public-file', NO-OF-DAYS defaults to
`planner-diary-public-number-of-days'.

Put this is your day pages:
\"<lisp>(planner-diary-public-entries-here)</lisp>\"
or this, if you want to do fancy things:
\"<lisp>(planner-diary-public-entries-here \"/path/to/diary/file\" 1)</lisp>\"

You might want to use `planner-day-page-template' to do so."
  (planner-diary-get-public-entries
   (planner-filename-to-calendar-date
    (planner-diary-get-name))
   (or file planner-diary-public-file)
   (or no-of-days planner-diary-public-number-of-days)))

(defun planner-diary-private-entries-here (&optional file no-of-days)
  "Display the diary appointments entries from FILE for the next NO-OF-DAYS.
FILE defaults to `planner-diary-private-file', NO-OF-DAYS defaults to
`planner-diary-private-number-of-days'.

Put this is your day pages:
\"<lisp>(planner-diary-private-entries-here)</lisp>\"
or this, if you want to do fancy things:
\"<lisp>(planner-diary-private-entries-here \"/path/to/diary/file\" 1)</lisp>\"

You might want to use `planner-day-page-template' to do so."
  (planner-diary-get-private-entries
   (planner-filename-to-calendar-date
    (planner-diary-get-name))
   (or file planner-diary-private-file)
   (or no-of-days planner-diary-private-number-of-days)))

;;; CODE FOR DEALING WITH SECTIONS
;; There's a lot of code duplication in the following 3 functions.
;;;###autoload
(defun planner-diary-update-section (file title text &optional force)
  ;;FIXME: Find a good place to insert a new section
  "Update FILE's existing section TITLE by replacing existing text with TEXT.
If optional arg FORCE is non-nil, update the section even if it doesn't exist,
i.e. insert TITLE followed by TEXT at the top of the buffer."
  ;; search for something like "^* Diary$", delete buffer content to the next
  ;; "^* "
  ;; sanity checks
  (unless (equal major-mode 'planner-mode)
    (error "This is not a planner buffer"))
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward (concat "^" title "$") (point-max) t)
        (when (or force planner-diary-create-section-flag)
          (insert title "\n\n\n")
          (backward-char 3)
          t)
        (error "No \"%s\" section in this buffer" title))
    ;; point is at the end of something like "* Diary"
    ;; delete the old text
    (let ((beg (point))
          (end (if (re-search-forward "^* " (point-max) t)
                   (progn (beginning-of-line)
                          (backward-char 1)
                          (point))
                 (point-max))))
      (delete-region beg end)
      ;; point is at the end of "* Diary"
      (insert "\n\n")
      (unless (string= text "")
          (insert text "\n")))))
   
(defun planner-diary-insert-diary (&optional force)
  "Insert the fancy diary for the day into the day plan file.
If FORCE is non-nil, insert a diary section even if there is no
`planner-diary-string' in the buffer."
  (interactive "P")
  ;; sanity check
  (let ((date (planner-diary-get-name)))
    (unless (string-match planner-date-regexp date)
      (error "Cannot insert diary in this buffer"))
    (planner-diary-update-section
     date ; file
     planner-diary-string ; title
     (planner-diary-get-diary-entries     ; text
      (planner-filename-to-calendar-date  ; date
       date)
      planner-diary-number-of-days
      planner-diary-file)
     force)))

(defun planner-diary-insert-diary-maybe (&optional force)
  "Maybe insert the fancy diary for the day into the day plan file.
If the current day is in the past and FORCE is nil, don't do anything.  If
FORCE is non-nil, insert a diary section even if there is no
`planner-diary-string' in the buffer."
  (interactive "P")
  (let ((date (planner-diary-get-name)))
    ;; check if we're in a day plan buffer
    (if (and (string-match planner-date-regexp date)
             (or force                    ; don't care about the date
                 (not (string< date (planner-today))))) ; not past
        ;; today, future, or force
        (planner-diary-insert-diary force)
      ;; we are in the past -> do nothing, message when interactive
      (when (interactive-p)
        (message "No day plan buffer or date is in the past.  No diary entries inserted.")))))
  
(defun planner-diary-insert-appts (&optional force)
  "Insert the diary appointments for the day into the day plan file.
If FORCE is non-nil, insert a diary appointments section even if there is no
`planner-diary-appts-string' in the buffer."
  (interactive "P")
  ;; sanity check
  (let ((date (planner-diary-get-name)))
    (unless (string-match planner-date-regexp date)
      (error "Cannot insert diary in this buffer"))
    (planner-diary-update-section
     date ; file
     planner-diary-appts-string ; title
     (planner-diary-get-appts-entries     ; text
      (planner-filename-to-calendar-date  ; date
       date)
      planner-diary-appts-number-of-days
      planner-diary-appts-file)
     force)))

(defun planner-diary-insert-appts-maybe (&optional force)
  "Maybe insert the diary appointments for the day into the day plan file.
If the current day is in the past and FORCE is nil, don't do anything.  If
FORCE is non-nil, insert a diary appointments section even if there is no
`planner-diary-appts-string' in the buffer."
  (interactive "P")
  ;; check if we're in a day plan buffer
  (let ((date (planner-diary-get-name)))
    (if (and (string-match planner-date-regexp date)
             (or force                    ; don't care about the date
                 (not (string< date (planner-today))))) ; not past
        ;; today, future, or force
        (planner-diary-insert-appts force)
      ;; we are in the past -> do nothing, message when interactive
      (when (interactive-p)
        (message "No day plan buffer or date is in the past.  No diary entries inserted.")))))

(defun planner-diary-insert-cal-desk (&optional force)
  "Insert the cal-desk diary for the day into the day plan file.
If FORCE is non-nil, insert a cal-desk diary section even if there is no
`planner-diary-cal-desk-string' in the buffer."
  (interactive "P")
  ;; sanity check
  (let ((date (planner-diary-get-name)))
    (unless (string-match planner-date-regexp date)
      (error "Cannot insert diary in this buffer"))
    (planner-diary-update-section
     date ; file
     planner-diary-cal-desk-string ; title
     (planner-diary-get-cal-desk-entries     ; text
      (planner-filename-to-calendar-date  ; date
       (file-name-nondirectory date))
      planner-diary-cal-desk-number-of-days
      planner-diary-cal-desk-file)
     force)))

(defun planner-diary-insert-cal-desk-maybe (&optional force)
  "Maybe insert the cal-desk diary for the day into the day plan file.
If the current day is in the past and FORCE is nil, don't do anything.  If
FORCE is non-nil, insert a cal-desk appointments section even if there is no
`planner-diary-cal-desk-string' in the buffer."
  (interactive "P")
  ;; check if we're in a day plan buffer
  (let ((date (planner-diary-get-name)))
    (if (and (string-match planner-date-regexp date)
             (or force                    ; don't care about the date
                 (not (string< date (planner-today))))) ; not past
        ;; today, future, or force
        (planner-diary-insert-cal-desk force)
      ;; we are in the past -> do nothing, message when interactive
      (when (interactive-p)
        (message "No day plan buffer or date is in the past.  No diary entries inserted.")))))


(defun planner-diary-insert-public (&optional force)
  "Insert the public diary for the day into the day plan file.
If FORCE is non-nil, insert a public diary section even if there is no
`planner-diary-public-string' in the buffer."
  (interactive "P")
  ;; sanity check
  (let ((date (planner-diary-get-name)))
    (unless (string-match planner-date-regexp date)
      (error "Cannot insert diary in this buffer"))
    (planner-diary-update-section
     date ; file
     planner-diary-public-string ; title
     (planner-diary-get-public-entries     ; text
      (planner-filename-to-calendar-date  ; date
       date)
      planner-diary-public-number-of-days
      planner-diary-public-file)
     force)))

(defun planner-diary-insert-public-maybe (&optional force)
  "Maybe insert the public diary for the day into the day plan file.
If the current day is in the past and FORCE is nil, don't do anything.  If
FORCE is non-nil, insert a public appointments section even if there is no
`planner-diary-public-string' in the buffer."
  (interactive "P")
  ;; check if we're in a day plan buffer
  (let ((date (planner-diary-get-name)))
    (if (and (string-match planner-date-regexp date)
             (or force                    ; don't care about the date
                 (not (string< date (planner-today))))) ; not past
        ;; today, future, or force
        (planner-diary-insert-public force)
      ;; we are in the past -> do nothing, message when interactive
      (when (interactive-p)
        (message "No day plan buffer or date is in the past.  No diary entries inserted.")))))

(defun planner-diary-insert-private (&optional force)
  "Insert the private diary for the day into the day plan file.
If FORCE is non-nil, insert a private diary section even if there is no
`planner-diary-private-string' in the buffer."
  (interactive "P")
  ;; sanity check
  (let ((date (planner-diary-get-name)))
    (unless (string-match planner-date-regexp date)
      (error "Cannot insert diary in this buffer"))
    (planner-diary-update-section
     date ; file
     planner-diary-private-string ; title
     (planner-diary-get-private-entries     ; text
      (planner-filename-to-calendar-date  ; date
       date)
      planner-diary-private-number-of-days
      planner-diary-private-file))
    force))

(defun planner-diary-insert-private-maybe (&optional force)
  "Maybe insert the private diary for the day into the day plan file.
If the current day is in the past and FORCE is nil, don't do anything.  If
FORCE is non-nil, insert a private appointments section even if there is no
`planner-diary-private-string' in the buffer."
  (interactive "P")
  ;; check if we're in a day plan buffer
  (let ((date (planner-diary-get-name)))
    (if (and (string-match planner-date-regexp date)
             (or force                    ; don't care about the date
                 (not (string< date (planner-today))))) ; not past
        ;; today, future, or force
        (planner-diary-insert-private force)
      ;; we are in the past -> do nothing, message when interactive
      (when (interactive-p)
        (message "No day plan buffer or date is in the past.  No diary entries inserted.")))))

;; UPDATE ALL DIARIES
;;;###autoload
(defun planner-diary-insert-all-diaries (&optional force)
  "Update all diary sections in a day plan file.
If FORCE is non-nil, insert a diary section even if there is no section header.
Inserts only diaries if the corresponding `planner-diary-use-*' variable is t."
  (interactive)
  (when planner-diary-use-diary
    (planner-diary-insert-diary force))
  (when planner-diary-use-cal-desk
    (planner-diary-insert-cal-desk force))
  (when planner-diary-use-appts
    (planner-diary-insert-appts force))
  (when planner-diary-use-private-diary
    (planner-diary-insert-private force))
  (when planner-diary-use-public-diary
    (planner-diary-insert-public force)))

;;;###autoload
(defun planner-diary-insert-all-diaries-maybe (&optional force)
  "Update all diary sections in a day plan file.
If the current day is in the past and FORCE is nil, don't do anything.
If FORCE is non-nil, insert a diary section even if there is no section header.
Inserts only diaries if the corresponding `planner-diary-use-*' variable is t."
  (interactive)
  ;; I intentionally call these individual functions rather than
  ;; planner-diary-insert-all-diaries.  It might make future code changes
  ;; simpler.
  (when planner-diary-use-diary
    (planner-diary-insert-diary-maybe force))
  (when planner-diary-use-cal-desk
    (planner-diary-insert-cal-desk-maybe force))
  (when planner-diary-use-appts
    (planner-diary-insert-appts-maybe force))
  (when planner-diary-use-private-diary
    (planner-diary-insert-private-maybe force))
  (when planner-diary-use-public-diary
    (planner-diary-insert-public-maybe force)))

;;;###autoload
(defun planner-diary-show-day-plan-or-diary ()
  "Show the day plan or diary entries for the date under point in calendar.
Add this to `calendar-move-hook' if you want to use it.  In that case you
should also `remove-hook' `planner-calendar-show' from `calendar-move-hook'."
  (interactive)
  (or (planner-calendar-show)
      (view-diary-entries 1)))

;;;###autoload
(defun planner-diary-insinuate ()
  "Hook Diary into Planner.
Automatically insert and update a Diary section in day plan files.
This adds a new key binding to `planner-mode-map':
C-cC-e updates the diary sections."
  ;; FIXME: update all diary sections: planner-diary-insert-all-diaries-maybe
  (define-key planner-mode-map "\C-c\C-e" 'planner-diary-insert-all-diaries-maybe)
  (add-hook 'planner-goto-hook 'planner-diary-insert-all-diaries-maybe))

;;;###autoload
(defalias 'planner-insinuate-diary 'planner-diary-insinuate)

;;;###autoload
(defun planner-diary-add-entry (date time text)
  "Prompt for a diary entry to add to `diary-file' on DATE.
Uses `planner-annotation-functions' to make hyperlinks.
TIME and TEXT are used in the description."
  (interactive (list (planner-read-date)
		     (read-string "Time: ")
                     (read-string "Diary entry: ")))
  (save-window-excursion
    (make-diary-entry
     (concat
      (let ((cal-date (planner-filename-to-calendar-date date)))
        (calendar-date-string cal-date t t))
      " " time " " text " "
      (run-hook-with-args-until-success
       'planner-annotation-functions))
     nil planner-diary-file)))

(provide 'planner-diary)

;;; planner-diary.el ends here
