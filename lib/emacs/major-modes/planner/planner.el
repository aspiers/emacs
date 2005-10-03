;;; planner.el --- The Emacs Planner

;;; Commentary:

;;;_* Commentary

;;;_ + Package description

;; Copyright (C) 2001 John Wiegley <johnw@gnu.org>
;; Copyright (C) 2003, 2004 Sandra Jean Chua <sacha@free.net.ph>
;; Parts copyright (C) 2004 David D. Smith (davidsmith AT acm DOT org)
;; Parts copyright (C) 2004 Yvonne Thomson (yvonne AT netbrains DOT com DOT au)
;; Parts copyright (C) 2004 Michael Olson (mwolson AT gnu DOT org)
;; Parts copyright (C) 2004 Maciej Kalisak (mac AT cs DOT toronto DOT edu)
;; Parts copyright (C) 2004 Chris Parsons (chris.p AT rsons.org)
;; Parts copyright (C) 2004 Stefan Reichör (stefan AT xsteve.at)
;; Parts copyright (C) 2004 Dale P. Smith (dsmith AT mail.actron.com)
;; Parts copyright (C) 2004 Dirk Bernhardt (nospam AT krid.de)
;; Parts copyright (C) 2004 Angus Lees (gus AT debian.org)

;; Emacs Lisp Archive Entry
;; Filename: planner.el
;; Version: 2005.08.20-17.59-stable
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Description: Use Emacs for life planning
;; URL: http://sacha.free.net.ph/notebook/emacs/planner/planner.el
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
;;
;;;_ + Usage
;;
;; Place planner.el in your load path and add this to your .emacs:
;;
;;    (require 'planner)
;;
;; By default and for backward compatibility, the following operations
;; do not have keybindings, and are only accessible from the Planner
;; menu:
;;
;;    planner-copy-or-move-region
;;    planner-delete-task
;;    planner-task-delegated
;;    planner-task-pending
;;    planner-fix-tasks
;;
;; You may find it easier to install keybindings for those operations by
;; inserting the following in your .emacs file:
;;
;;   ;; Note: This changes some of the default key bindings for planner-mode
;;   (planner-install-extra-task-keybindings)
;;
;; If you want to change `planner-directory' and some other variables,
;; either use Customize or use `planner-option-customized'. For
;; example:
;;
;;    (planner-option-customized 'planner-directory "~/Plans")
;;    (planner-option-customized 'planner-publishing-directory
;;                               "~/public_html/plans")
;;
;; and if you want to modify other emacs-wiki variables:
;;
;;    (add-to-list 'planner-custom-variables
;;                 '(some-emacs-wiki-variable . "some-emacs-wiki-value"))
;;    (planner-option-customized 'planner-custom-variables
;;                               planner-custom-variables)
;;
;; See `emacs-wiki-update-project' and `planner-custom-variables' for more
;; details.
;;
;; You can customize Planner. M-x customize-group RET planner RET
;; or see the Options section.
;;
;;; Note:
;;
;; This package extends emacs-wiki.el to act as a day planner, roughly
;; equivalent to the one used by Franklin-Covey.  If they have patents
;; and trademarks and copyrights to prevent me even thinking in terms
;; of their methodology, then I can't believe they care at all about
;; productivity.
;;
;;;_ + Summary
;;
;; * Make a planning file
;;
;; Open a wiki file within your planning directory.  By default,
;; planner-directory is set to "~/Plans". You may have to use C-x C-f
;; to open the file.
;;
;; A plan file generally describes a long-term plan. For example, you
;; could make a plan file for your ThesisProject or your
;; ContinuousLearning. Planner.el can help you organize related ideas,
;; tasks and resources into a coherent plan.
;;
;; * Break your plan into stages
;;
;; Start the file with your "vision", or the goal you intend to
;; accomplish.  Break this up into parts, and create a Wiki file for
;; each part, with defined milestones which constitute the "goal" for
;; that part.
;;
;; * Write out the tasks for each stage
;;
;; In each sub-plan, list out the tasks necessary to accomplish the
;; milestone.  Write them into the file like this:
;;
;; #A _ 1h Call so and so to make a reservation
;;
;; * Decide on a priority for each task
;;
;; The A is the priority of the task.  The _ means it isn't done yet,
;; and the 1h is a quick estimate on how long it will task.  The time
;; estimates are optional.
;;
;; The priorities break down like this:
;;
;;  A: if you don't do it, your plan will be compromised, and you
;;     will have to either abort, backtrack, or make profuse apologies
;;     to someone
;;
;;  B: if you don't do it, your plan will be delayed
;;
;;  C: the plan won't be complete until it's done, but there's no
;;     pressure to do it now
;;
;; * Schedule the tasks
;;
;; Put your cursor on a line containing a task, and type C-c C-c.
;; This will copy the task to a specific day, which you will be
;; prompted for.  The Emacs Calendar pops up, so you can pick a free
;; day (if you use the Emacs diary and appointment system, the
;; Calendar is even more useful).
;;
;; You will now see your new task, with a link back to your planning
;; page.  Selecting this link will take you back to that task on the
;; planning page, where you will see that the planning page's task now
;; has a link to the particular day you scheduled the task for.
;;
;; The two tasks (the one on the planning page, and the one on the
;; daily task list) are linked.  Changing the status of one (using C-c
;; C-x, or C-c C-s, for example) will change the status of the other.
;; If you forward the task to another day (using C-c C-c on the daily
;; task page), the planning page's link will be updated to refer to
;; the new day.  This is so that you can focus on your daily task list
;; during the day, but see an overview of your plan's progress at any
;; time.
;;
;; * Do the work
;;
;; That's it, as far as what planner.el can do.  As you complete tasks
;; each day, they will disappear from view.  This only happens for
;; today's completed and forwarded tasks.
;;
;; Planning is an art, just as estimating time is an art.  It happens
;; with practice, and by thinking about these things.  The Commentary
;; below provides a few of my own thoughts on the matter, although I
;; will say that this an art I have yet to truly develop.
;;
;; http://sacha.free.net.ph/notebook/emacs/planner/README has John Wiegley's
;; original commentary.
;;
;;;_ + And now back to technical matters
;;
;; In order to refresh and renumber all of your tasks according to their
;; actual order in the buffer, simply save the file or call
;; M-x planner-fix-tasks .
;;
;; Here is a summary of the keystrokes available, including a few I
;; did not mention:
;;
;;   M-x plan  Begin your planning session.  This goes to the last
;;             day for which there is any planning info (or today if
;;             none), allowing you to review, and create/move tasks
;;             from that day.
;;
;;   C-M-p     Raise a task's priority
;;   C-M-n     Lower a task's priority
;;
;;   C-c C-s   Mark the task as in progress or delegated
;;   C-c C-x   Mark the task as finished
;;
;;   C-c C-t   Create a task associated with the current Wiki page
;;             If you are on the opening line of a Note entry, it is
;;             assume that the note itself is the origin of the task.
;;   C-c C-c   Move or copy the current task to another date
;;             If the current task is an original (meaning you are in
;;             the buffer where's defined, hopefully a planning page)
;;             then it will be copied, and the original task will also
;;             now point to the copy.  If the current task is a copy,
;;             it will just be moved to the new day, and the original
;;             tasks link will be updated.
;;
;;   C-c C-n   Jump to today's task page
;;
;; If you call (planner-calendar-insinuate), typing 'n' in the Emacs
;; calendar will jump to today's task page.
;;
;;;_ + Planning and schedules
;;
;; Sometimes you will have appointments during the day to schedule,
;; which "block out" time that might otherwise be spent on tasks.
;; Users are encouraged to use the Emacs Calendar for this, along with
;; Diary Mode (see the Emacs manual)
;;.
;;
;; However, there is a way to do scheduling directly in planner-mode.
;; It requires the external tool "remind" (Debian users type "apt-get
;; install remind".  All others go to
;; http://www.roaringpenguin.com/penguin/open_source_remind.php)
;;
;; Once you have remind installed, you will need two scripts in your
;; local bin directory (/usr/local/bin, $HOME/bin, wherever).  These
;; scripts can be downloaded from my web site:
;;
;;   http://sacha.free.net.ph/notebook/emacs/plan2rem
;;   http://sacha.free.net.ph/notebook/emacs/rem2diary
;;
;; Also, download
;;
;;   http://sacha.free.net.ph/notebook/emacs/remind.el
;;
;; and put it somewhere in your load path. Take a look at remind.el
;; for more details. You will need to edit a few things to get it
;; to work.

;; Lastly, here is another snippet for your .emacs file.  It creates a
;; keybinding in planner-mode, C-c C-w, which jumps you to the
;; Schedule section of that file.

;; (defun planner-goto-schedule ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (unless (re-search-forward "^\\* Schedule\n\n" nil t)
;;     (re-search-forward "^\\* Notes")
;;     (beginning-of-line)
;;     (insert "* Schedule\n\n\n\n")
;;     (forward-line -2)))
;;
;; (eval-after-load "planner"
;;   '(progn
;;      (define-key planner-mode-map [(control ?c) (control ?w)]
;;        'planner-goto-schedule)))

;; The contents of a scheduling section look like this, which is
;; rendered in HTML as a table:
;;
;;   * Schedule
;;
;;    8:00 | Wake up
;;   14:00 | Go to the dentist (2:00)
;;   18:00 | Watch TV
;;
;; The start time is given in 24-hour time, with an optional duration
;; occuring in parentheses at the end of the description hs-show(in
;; HOURS:MINUTES).  And off you go!
;;
;; You can also organize this as
;;
;;   8:00 |  8:30 | Wake up
;;  14:00 | 16:00 | Go to the dentist
;;  18:00 | 21:00 | Watch TV
;;
;;;_ + Example planning file
;;
;; The format of a planning file is given below.  You are responsible
;; for keeping it looking like this.  I intentionally did not make
;; planner.el heavy on the UI side of things, too keep it more
;; free-form and open.  This lets you adapt it to whatever your
;; particular preferences might be.
;;
;;----------------------------------------------------------------------
;; * Tasks
;;
;; #A1 _ An open task, very important!
;; #A2 X A closed task (MyPlan)
;; #A3 o A task that's delayed, or delegated (MyPlan)
;;
;; * Notes
;;
;; .#1 This is note number one
;;
;; Notes on note number one!
;;
;; .#2 This weird ".#2" syntax is used because it's what allout.el
;;     likes for enumerated lists, and it makes using
;;     outline-minor-mode (with allout) very handy.  You can omit the
;;     leading period if you like, though.  It's optional.
;;
;; ----------------------------------------------------------------------
;;
;;;_ + Other packages you can use with planner
;;
;; planner-bbdb.el       | Link to your contacts
;; planner-diary.el      | Thomas Gehrlein's diary integration
;; planner-gnus.el       | Link to your mail/news messages
;; planner-id.el         | Automatically add unique task IDs
;; planner-notes.el      | Create a note index
;; planner-rss.el        | Publish your notes as an RSS feed
;; planner-schedule.el   | Estimate task completion time
;; planner-timeclock.el  | Clock in and clock out
;; planner-w3m.el        | Make tasks based on W3M buffers
;; remember.el           | Easily remember short notes
;;
;; All are available at
;; http://sacha.free.net.ph/notebook/emacs/emacs-wiki/

;;;_ + Thanks

;; A short, partial list of contributors, those who reported bugs, and
;; those who gave valuable suggestions can be found at
;; http://sacha.free.net.ph/notebook/wiki/PlannerMode.php

;;;_ + Contributors

;; David D. Smith (davidsmith AT acm DOT org) helped links to planner
;; pages be created properly, among other things.

;; Daniel Neri (dne AT mayonnaise DOT net) fixed a couple of typos.

;; Mario Peter (email address unknown) made
;; `planner-in-progress-task-face' use :bold instead of :slant if
;; using XEmacs.

;; Yvonne Thomson (yvonne AT netbrains DOT com DOT au) contributed
;; `planner-annotation-from-info'.

;; Hoan Ton-That (hoan AT ton-that DOT org) had the idea to strip the
;; directory from planner file annotations and contributed the base
;; patch.

;; Michael Olson (mwolson AT gnu DOT org) contributed XHTML 1.1
;; patches, fixed some bugs that irked him, and did a few other
;; miscellaneous things.

;; Maciej Kalisiak (mac AT cs DOT toronto DOT edu) made a patch that
;; sorts dated tasks before undated ones.  Maciej also helped with the
;; separation of the sorting and renumbering processes.

;; Dale P. Smith (email address unknown) contributed a small patch
;; that fixes tasks that are not true wiki names.

;; Stefan Reichör (stefan AT xsteve DOT at) contributed a small patch
;; that saves only modified buffers.

;; Peter K. Lee (saint AT corenova DOT com) fixed a few initial errors
;; with missing and malformed functions like `planner-page-exists-p'
;; and `planner-option-customized'

;;; Code:

;;;_* Prerequisites

(require 'emacs-wiki)
(require 'sort)
(require 'calendar)
(require 'font-lock)
(require 'info)
(require 'easymenu)
(when (featurep 'xemacs)
  (require 'derived)
  (require 'overlay))

(defvar planner-loaded nil)
;; Not very useful now; FIXME figure out how to get sane
;; version numbers out of arch
(defvar planner-version "2005.08.20-17.59-stable"
  "Version of this planner file, for easy reference.")
(defvar planner-regexp-space emacs-wiki-regexp-space
  "Space for regular expressions.")

(defun planner-option-customized (sym val)
  "Set SYM to VAL and update the WikiPlanner project."
  (set sym val)
  (when planner-loaded
    (planner-update-wiki-project)))

;;;_* Options

(defgroup planner nil
  "A personal information manager for Emacs."
  :prefix "planner-"
  :group 'applications)

(defcustom planner-project-default-name "WikiPlanner"
  "The name of this project in `emacs-wiki-projects'.

This is used by `planner-update-wiki-project' to make sure that any
old entries are removed correctly."
  :type 'string
  :group 'planner)

(defcustom planner-project planner-project-default-name
  "The name of this project, used when referencing it from other
Emacs Wiki projects."
   :type 'string
   :group 'planner)

(defcustom planner-directory "~/Plans"
  "The directory that contains your planning files."
  :require 'planner
  :type 'directory
  :set 'planner-option-customized
  :group 'planner)

(defcustom planner-publishing-directory emacs-wiki-publishing-directory
  "The directory where the planner wiki is published to."
  :type 'directory
  :set 'planner-option-customized
  :group 'planner)

(defcustom planner-publish-dates-first-p nil
  "Non-nil means put day pages at the top of the index."
  :type 'boolean
  :group 'planner)

(defcustom planner-use-day-pages t
  "If non-nil, allow the use of day pages.
You can set this to nil if you use plan pages exclusively and
don't want to be prompted for dates. If so, then `plan'
will bring up the WelcomePage of your planner wiki."
  :type 'boolean
  :group 'planner)

(defcustom planner-use-plan-pages t
  "If non-nil, allow the use of plan pages.
You can set this to nil if you use day pages exclusively and
don't want to be prompted for plans."
  :type 'boolean
  :group 'planner)

(defcustom planner-mode-hook nil
  "A hook for Planner mode."
  :type 'hook
  :group 'planner)

(defcustom planner-annotation-functions
  '(planner-annotation-from-planner-note
    planner-annotation-from-planner
    planner-annotation-from-wiki
    planner-annotation-from-dired
    planner-annotation-from-file-with-position)
  "Functions tried in order by `planner-create-task-from-buffer'.
To change the behavior of `planner-create-task-from-buffer',
remove, change the order of, or insert functions in this list."
  :type 'hook
  :group 'planner)

(defcustom planner-annotation-symbol-string "{}"
  "The string to be replaced by annotation from `planner-annotation-functions'.
If nil or not found in the task title, the annotation will be
added to the end."
  :type 'string
  :group 'planner)

(defcustom planner-use-other-window t
  "If non-nil, Planner will open planner files in another window."
  :type 'boolean
  :group 'planner)

(defcustom planner-show-only-existing t
  "If non-nil, `planner-show' only shows existing files."
  :type 'boolean
  :group 'planner)

(defcustom planner-reverse-chronological-notes t
  "*If non-nil, notes are added to the beginning of the section."
  :type 'boolean
  :group 'planner)

(defcustom planner-create-section-function 'planner-create-at-top
  "Called when creating a new section.
Some functions you can use are `planner-create-at-top' and
`planner-create-at-bottom'."
  :type 'function
  :group 'planner)

(defcustom planner-template-fuzz-factor 5
  "Controls the fuzziness of `planner-page-default-p'.
Right now, this is the number of additional characters over
`planner-day-page-template' allowed in a buffer before
`planner-page-default-p' assumes it has been modified."
  :type 'integer
  :group 'planner)

(defcustom planner-calendar-show-planner-files t
  "If non-nil, shows a plan file every time a day is selected in Calendar."
  :type 'boolean
  :group 'planner)

(defcustom planner-day-page-template
  "* Tasks\n\n\n* Schedule\n\n\n* Notes\n\n\n"
  "Template to be inserted into blank daily pages.
If this is a string, it will be inserted into the blank page.  If
this is a function, it will be called with no arguments from a
blank planner page and should insert the template.

If you want to change the name of special sections like Tasks and Notes,
update the `planner-sections' option as well."
  :type '(choice
          (string :tag "Template")
          (function :tag "Function"))
  :group 'planner)

(defcustom planner-plan-page-template "* Tasks\n\n\n* Notes\n\n\n"
  "Template to be inserted into blank plan pages.
If this is a string, it will be inserted into the blank page.  If
this is a function, it will be called with no arguments from a
blank planner page and should insert the template.

If you want to change the name of special sections like Tasks and Notes,
update the `planner-sections' option as well."
  :type '(choice
          (string :tag "Template")
          (function :tag "Function"))
  :group 'planner)

(defcustom planner-default-section 'tasks
  "Default section when you use `planner-goto' to open a page.
If this is a string, it should be a section name. If this is a symbol,
the section name is looked up in `planner-sections'."
  :type '(choice (string :tag "String")
		 (symbol :tag "Symbol"))
  :group 'planner)

(defcustom planner-sections '((tasks . "Tasks")
			      (notes . "Notes"))
  "Special sections in pages.
This option makes it easier to change the names of your sections
without modifying a lot of Planner code. If you change this, you
may also want to change `planner-day-page-template' and
`planner-plan-page-template'. You normally don't need to change
these, though."
  :type '(alist :key symbol :value string)
  :group 'planner)

(defcustom planner-ignored-from-addresses
  (and user-mail-address
       (not (string= user-mail-address ""))
       (regexp-quote user-mail-address))
  "Regexp of From headers that may be suppressed in favor of To headers."
  :group 'planner
  :type 'regexp)

(defcustom planner-dates-relative-to-today-flag nil
  "Non-nil means relative dates (+1, -1) are always based on today.
By default, dates are based on the current page."
  :group 'planner
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup planner-tasks nil
  "Planner options related to tasks."
  :prefix "planner-"
  :group 'planner)

(defcustom planner-carry-tasks-forward 3
  "If non-nil, carry unfinished tasks forward automatically.
If a positive integer, scan that number of days in the past.
If 0, scan all days for unfinished tasks.
If t, scan one day in the past (old behavior).
If nil, do not carry unfinished tasks forward."
  :type '(choice
          (const :tag "Scan all days" 0)
          (const :tag "Scan most recent day" t)
          (const :tag "Do not carry tasks forward" nil)
          (integer :tag "Number of days to scan"))
  :group 'planner-tasks)

(defcustom planner-marks-regexp "[_oX>CP]"
  "Regexp that matches status character for a task.
If you change this, also change `planner-publishing-markup'."
  :type 'regexp
  :group 'planner-tasks)

(defcustom planner-default-task-priority "B"
  "Default priority for new tasks created with `planner-create-task'."
  :type 'string
  :group 'planner-tasks)

(defcustom planner-default-task-status "_"
  "Default status for new tasks created with `planner-create-task'."
  :type 'string
  :group 'planner-tasks)

(defcustom planner-add-task-at-end-flag nil
  "*Non-nil means create tasks at the bottom of the first task block."
  :group 'planner-tasks
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-wiki options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup planner-emacs-wiki nil
  "Planner.el user options for integration with emacs-wiki.el"
  :prefix "planner-"
  :group 'planner)

(defcustom planner-name-regexp
  (when planner-use-day-pages
    (concat "[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]#[A-Za-z0-9_%]+"
            "\\|[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]"))
  "A regexp used to match planner references in a planning buffer."
  :type 'regexp
  :set 'planner-option-customized
  :group 'planner-emacs-wiki)

(defcustom planner-custom-variables nil
  "A list of planner-specific Emacs-Wiki variable settings.
You can customize any emacs-wiki variable to be used specially within
planner mode buffers, except for the following, whose values are
derived from the other planner mode customized variables:

  `emacs-wiki-directories'
  `emacs-wiki-major-mode'
  `emacs-wiki-markup-tags'
  `emacs-wiki-publishing-markup'
  `emacs-wiki-url-regexp'
  `emacs-wiki-name-regexp'
  `emacs-wiki-url-or-name-regexp'
  `emacs-wiki-highlight-regexp'

If you want to customize the derived variables, you can set them from
`planner-mode-hook'."
  :type `(repeat
          (choice
           (cons :tag "emacs-wiki-predicate"
                 (const emacs-wiki-predicate) function)
           (cons :tag "emacs-wiki-project-server-prefix"
                 (const emacs-wiki-project-server-prefix) string)
           ,@(mapcar
              (function
               (lambda (sym)
                 (list 'cons :tag (symbol-name sym)
                       (list 'const sym)
                       (get sym 'custom-type))))
              (apropos-internal "\\`emacs-wiki-"
                                (function
                                 (lambda (sym)
                                   (get sym 'custom-type)))))))
  :set 'planner-option-customized
  :group 'planner-emacs-wiki)

(defcustom planner-publishing-markup
  ;; Update this if you change `planner-marks-regexp'
  '([emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags]
    ["^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oX>CP]\\)\\s-*\\(.+\\)"
     0 planner-markup-task]
    ["^\\.#\\([0-9]+\\)" 0 planner-markup-note])
  "List of additional markup rules to apply when publishing planner pages.
These rules are performed first, before any emacs-wiki rules.
See the docs for `emacs-wiki-publishing-markup' for more info."
  :type '(repeat
          (vector :tag "Markup rule"
                  (choice regexp symbol)
                  integer
                  (choice string function symbol)))
  :group 'planner-emacs-wiki)

(defcustom planner-markup-tags
  '(("notes" nil nil nil planner-notes-tag)
    ("past-notes" nil t nil planner-past-notes-tag)
    ("tasks" nil t nil planner-tasks-tag))
  "A list of tag specifications used for marking up planner pages.
See the documentation for `emacs-wiki-markup-tags'."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :set 'planner-option-customized
  :group 'planner-emacs-wiki)

;;;_* Keybindings


(defvar planner-mode-map
  (let ((map (copy-keymap emacs-wiki-mode-map)))
    (define-key map "\C-c\C-n" 'planner-goto-today)

    ;; moving between daily pages C-c C-j for goto (used to be C-g,
    ;; but that was confusing)
    (define-key map "\C-c\C-j\C-d" 'planner-goto) ; goto date
    (when planner-use-day-pages
      (define-key map "\C-c\C-j\C-p" 'planner-goto-previous-daily-page)
      (define-key map "\C-c\C-j\C-n" 'planner-goto-next-daily-page)
      (define-key map "\C-c\C-j\C-j" 'planner-goto-today) ; for easy typing
      (define-key map "\C-c\C-j\C-y" 'planner-goto-yesterday)
      (define-key map "\C-c\C-j\C-t" 'planner-goto-tomorrow)
      (define-key map "\C-c\C-j\C-r" 'planner-goto-most-recent)) ; recent

    (define-key map "\C-c\C-t" 'planner-create-task-from-buffer)
    (define-key map "\C-c\C-c" 'planner-copy-or-move-task)
    (define-key map "\C-c\C-u" 'planner-raise-task)
    (define-key map "\C-c\C-d" 'planner-lower-task)

    (define-key map "\M-p" 'planner-raise-task)
    (define-key map "\M-n" 'planner-lower-task)

    (define-key map "\M-\C-p" 'planner-raise-task-priority)
    (define-key map "\M-\C-n" 'planner-lower-task-priority)

    (define-key map "\C-c\C-z" 'planner-task-in-progress)
    (define-key map "\C-c\C-x" 'planner-task-done)
    (define-key map '[(control ?c) (control ?X)] 'planner-task-cancelled)
    map)
  "Keymap used by Planner mode.")

(defun planner-install-extra-context-keybindings ()
  "Install extra context-sensitive keybindings.
These keybindings conflict with windmove.el, but might
be useful.

On a task or note, the following keys will move around:

Shift-up: `planner-move-up'
Shift-down: `planner-move-down'
Shift-right: `planner-jump-to-link'"
  (interactive)
  (let ((map planner-mode-map))
    (define-key map [(shift up)] 'planner-move-up)
    (define-key map [(shift down)] 'planner-move-down)
    (define-key map [(shift right)] 'planner-jump-to-link)))

;;; Additional keybindings thanks to Thomas Gehrlein

(defun planner-install-extra-task-keybindings ()
  "Install additional task key bindings.
Warning! Overwrites some standard key bindings. See function
definition for keys added."
  (let ((map planner-mode-map))
    (define-key map "\C-c\C-t" nil)
    (define-key map "\C-c\C-t\C-t" 'planner-create-task-from-buffer)
    (define-key map "\C-c\C-t\C-k" 'planner-delete-task)
    (define-key map "\C-c\C-t\C-u" 'planner-update-task)
    (define-key map "\C-c\C-t\C-c" 'planner-copy-or-move-task)
    (define-key map '[(control ?c) (control ?t) (control ?C)]
      'planner-copy-or-move-region)
    (define-key map "\C-c\C-t\C-x" 'planner-task-done)
    (define-key map '[(control ?c) (control ?t) (control ?X)]
      'planner-task-cancelled)
    (define-key map "\C-c\C-t\C-d" 'planner-task-delegated)
    (define-key map "\C-c\C-t\C-p" 'planner-task-pending)
    (define-key map "\C-c\C-t\C-o" 'planner-task-in-progress)
    (define-key map "\C-c\C-t\C-r" 'planner-raise-task)
    (define-key map "\C-c\C-t\C-l" 'planner-lower-task)
    (define-key map "\C-c\C-t\C-n" 'planner-fix-tasks)))

;;; We need some keybindings for note-related functions, too

(defun planner-install-extra-note-keybindings ()
  "Install additional note-related key bindings.
See function definition for keys added."
  (let ((map planner-mode-map))
    (define-key map "\C-c\C-o" nil)
    (define-key map "\C-c\C-o\C-o" 'planner-create-note)
    (define-key map "\C-c\C-o\C-s" 'planner-search-notes)
    (define-key map "\C-c\C-o\C-b" 'planner-search-notes-with-body)
    (define-key map "\C-c\C-o\C-n" 'planner-renumber-notes)))

;;;_* Menu

;;; Menu thanks to Thomas Gehrlein
(easy-menu-define planner-menu planner-mode-map
  "Menu of planner mode.
See `planner-install-extra-task-keybindings' for additional bindings
you can use."
  (list
   "Planner"
   ;; moving between day plan pages
   (if planner-use-day-pages
       '("Goto"
         ["Plan page" planner-goto-plan-page]
         ["Date" planner-goto]
         ["Previous page" planner-goto-previous-daily-page]
         ["Next page" planner-goto-next-daily-page]
         ["Today" planner-goto-today]
         ;; do the next two make sense in a menu?
         ["Yesterday" planner-goto-yesterday]
         ["Tomorrow" planner-goto-tomorrow]
         ["Most recent" planner-goto-most-recent])
     '["Goto plan page" planner-goto-plan-page])
   ;; handling tasks
   '("Tasks"
     ["Create" planner-create-task-from-buffer]
     ["Create from note" planner-create-task-from-note]
     ["Delete" planner-delete-task]
     ["Update" planner-update-task]
     ["Copy or move task" planner-copy-or-move-task]
     ["Copy or move region" planner-copy-or-move-region]
     "---"
     ;; Roughly arranged by frequency, not by chronological sequence
     ["Mark \"done\"" planner-task-done]
     ["Mark \"delegated\"" planner-task-delegated]
     ["Mark \"pending\"" planner-task-pending]
     ["Mark \"in progress\"" planner-task-in-progress]
     ["Mark \"cancelled\"" planner-task-cancelled]
     ["Mark \"open\"" planner-task-open]
     "---"
     ["Raise task priority" planner-raise-task-priority]
     ["Lower task priority" planner-lower-task-priority]
     ["Format tasks nicely" planner-fix-tasks])
   ;; notes
   '("Notes"
     ["Create" planner-create-note]
     ["Create from task" planner-create-note-from-task]
     "---"
     ["Search" planner-search-notes]
     ["Search with body" planner-search-notes-with-body]
     ["Renumber" planner-renumber-notes])
   "---"
   ;; miscellaneous
   '["Plan" plan]
   "---"
   ;; help/info (now that we have a manual, use it)
   '["Info manual" (info "planner-el")]))

;;;_* Internal functions

;;;_ + Compatibility

;;;_  + Emacs vs XEmacs

(defun planner-derived-mode-p (&rest modes)
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (if (fboundp 'derived-mode-p)
      (apply 'derived-mode-p modes)
    ;; PUBLIC: find if the current mode derives from another.
    ;; Taken from GNU Emacs 21 subr.el
    (let ((parent major-mode))
      (while (and (not (memq parent modes))
                  (setq parent (get parent 'derived-mode-parent))))
      parent)))

(defun planner-replace-regexp-in-string (regexp replacement text &optional fixedcase literal )
  "Replace REGEXP with REPLACEMENT in TEXT.
If fourth arg FIXEDCASE is non-nil, do not alter case of replacement text.
If fifth arg LITERAL is non-nil, insert REPLACEMENT literally."
  (cond
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp replacement text fixedcase literal))
   ((fboundp 'replace-in-string)
    (replace-in-string text regexp replacement literal))
   (t (while (string-match regexp text)
        (setq text (replace-match replacement fixedcase literal text)))
      text)))

(defun planner-line-end-position (&optional n)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
See `line-end-position' for more details."
  (if (fboundp 'line-end-position)
      (line-end-position n)
    (save-excursion (end-of-line n) (point))))

(defun planner-line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
With argument n not nil or 1, move forward n - 1 lines first.
See `line-beginning-position' for more details."
  (if (fboundp 'line-beginning-position)
      (line-beginning-position n)
    (save-excursion (beginning-of-line n) (point))))

(defun planner-match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
string should be given if the last search was by `string-match' on STRING."
  (if (fboundp 'match-string-no-properties)
      (match-string-no-properties num string)
    (match-string num string)))

;;; Copied from subr.el
(defun planner-copy-overlay (o)
  "Return a copy of overlay O."
  (if (fboundp 'copy-overlay)
      (copy-overlay o)
    (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
                            ;; FIXME: there's no easy way to find the
                            ;; insertion-type of the two markers.
                            (overlay-buffer o)))
          (props (overlay-properties o)))
      (while props
        (overlay-put o1 (pop props) (pop props)))
      o1)))

;;; Copied from subr.el
(defun planner-remove-overlays (beg end name val)
  "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and or split."
  (if (fboundp 'remove-overlays)
      (remove-overlays beg end name val)
    (if (< end beg)
        (setq beg (prog1 end (setq end beg))))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (eq (overlay-get o name) val)
          ;; Either push this overlay outside beg...end
          ;; or split it to exclude beg...end
          ;; or delete it entirely (if it is contained in beg...end).
          (if (< (overlay-start o) beg)
              (if (> (overlay-end o) end)
                  (progn
                    (move-overlay (planner-copy-overlay o)
                                  (overlay-start o) beg)
                    (move-overlay o end (overlay-end o)))
                (move-overlay o (overlay-start o) beg))
            (if (> (overlay-end o) end)
                (move-overlay o end (overlay-end o))
              (delete-overlay o))))))))

(defun planner-unhighlight-region (begin end &optional verbose)
  "Remove all visual highlights in the buffer (except font-lock)."
  (planner-zap-overlays begin end)
  (emacs-wiki-unhighlight-region begin end verbose))

(defun planner-zap-overlays (beg end &optional verbose)
  "Remove all the planner-related overlays/extents from BEG to END."
  (if (featurep 'xemacs)
      (mapcar-extents 'delete-extent nil nil beg end nil 'planner t)
    (planner-remove-overlays beg end 'planner t)))

;;;_  + Emacs-wiki layer

(defmacro with-planner (&rest body)
  "Make sure BODY is evaluated in a `planner-mode' buffer."
  `(if (planner-derived-mode-p 'planner-mode)
       (progn ,@body)
     (with-emacs-wiki-project planner-project
       ,@body)))
(put 'with-planner 'lisp-indent-function 0)
(put 'with-planner 'edebug-form-spec '(body))

(defun planner-current-file ()
  "Return the file that is currently being published."
  ;; We define this as a function to get around the lack of
  ;; defvaralias in Emacs21.
  (or (and (boundp 'emacs-wiki-current-file)
           emacs-wiki-current-file)
      buffer-file-name))

(defun planner-file-alist (&optional no-check-p pages)
  "Return possible Wiki filenames in `planner-project'.
On UNIX, this list is only updated if one of the directories'
contents have changed or NO-CHECK-P is non-nil. On Windows, it is
always reread from disk."
  (with-planner
    (let ((result (emacs-wiki-file-alist no-check-p)))
      (if (and pages (not (eq pages t)))
          (delq nil
                (mapcar
                 (lambda (item)
                   (and (if (listp pages)
                            (member (car item) pages)
                          (string-match pages (cdr item)))
                        item))
                 result))
        result))))

;; This ugly workaround is necessary because with-temp-buffer
;; resets the buffer to the previous one after closing the temporary
;; buffer. That causes emacs-wiki-find-file to have no effect.
;; To work around this problem, we save the new buffer reference
;; and set the current buffer to it only if the command to open
;; the buffer actually changed the buffer.
(defun planner-find-file (wiki &optional command directory)
  "Open the Planner page WIKI by name.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the Wiki
page will be created if it does not already exist."
  (if (planner-derived-mode-p 'planner-mode)
      (emacs-wiki-find-file (planner-link-base wiki) command directory)
    (let (new-buffer
          old-buffer
          (emacs-wiki-project planner-project))
      (with-temp-buffer
        (setq old-buffer (current-buffer))
        (emacs-wiki-change-project planner-project)
        (emacs-wiki-find-file (planner-link-base wiki) command directory)
        (setq new-buffer (current-buffer))
        (when (equal old-buffer new-buffer)
          (setq new-buffer nil)))
      (when new-buffer (set-buffer new-buffer)))))

(defalias 'planner-page-name 'emacs-wiki-page-name)
(defalias 'planner-link-base 'emacs-wiki-wiki-base)
(defalias 'planner-link-name 'emacs-wiki-wiki-visible-name)
(defalias 'planner-time-less-p 'emacs-wiki-time-less-p)
(defalias 'planner-private-p 'emacs-wiki-private-p)
(defalias 'planner-published-file 'emacs-wiki-published-file)
(defalias 'planner-follow-name-at-point 'emacs-wiki-follow-name-at-point)
(defalias 'planner-next-reference 'emacs-wiki-next-reference)
(defalias 'planner-previous-reference 'emacs-wiki-previous-reference)
(defalias 'planner-visit-link 'emacs-wiki-visit-link)
(defalias 'planner-link-target 'emacs-wiki-wiki-link-target)

;; Function for compatibility with Muse
(defun planner-directory ()
  "Return the directory where Planner finds files."
  planner-directory)


;; Copied from w3m-url-encode-string (w3m.el)
(defun planner-encode-url (str &optional coding)
  "Hexify dangerous characters in STR.
If CODING is used, use that coding system."
  (save-match-data
    (apply (function concat)
	   (mapcar
	    (lambda (ch)
	      (cond
	       ((eq ch ?\n)             ; newline
		"%0D%0A")
	       ;; xxx?
	       ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch))
		(char-to-string ch))    ; printable
	       ((char-equal ch ?\x20)   ; space
		"%20")
	       (t
		(format "%%%02x" ch)))) ; escape
	    ;; Coerce a string to a list of chars.
	    (append (encode-coding-string
		     str
		     (or coding
			 (if (boundp 'emacs-wiki-coding-default)
			     emacs-wiki-coding-default)
			 'utf-8))
		    nil)))))

(defun planner-make-link (link &optional name single)
  "Return a Wiki link to LINK with NAME as the text.
If SINGLE is non-nil, treat it as a single link.
If LINK is already a valid link, replace it's description
by NAME"
  (if (or (null link) (string= link ""))
      ""
    (emacs-wiki-make-link link name)))

(defun planner-link-escape (text)
  "Escape dangerous characters in TEXT."
  (when text
    (while (string-match "\\[" text)
      (replace-match "%5B" t t text))
    (while (string-match "\\]" text)
      (replace-match "%5D" t t text))
    text))

(defun planner-link-unescape (text)
  "Escape dangerous characters in TEXT."
  (when text
    (while (string-match "%5B" text)
      (replace-match "[" t t text))
    (while (string-match "%5D" text)
      (replace-match "]" t t text))
    text))

;;;_ + Mode

(defcustom planner-align-tasks-automatically t
  "Non-nil means align tasks whenever a planner file is saved."
  :type 'boolean
  :group 'planner)
(defcustom planner-sort-tasks-automatically t
  "Non-nil means sort tasks whenever a planner file is saved."
  :type 'boolean
  :group 'planner)
(defcustom planner-renumber-tasks-automatically nil
  "Non-nil means renumber tasks whenever a planner file is saved."
  :type 'boolean
  :group 'planner)
(defcustom planner-renumber-notes-automatically nil
  "Non-nil means renumber notes whenever a planner file is saved."
  :type 'boolean
  :group 'planner)

;;;###autoload

(define-derived-mode planner-mode emacs-wiki-mode "Planner"
  "A personal information manager for Emacs.
\\{planner-mode-map}"
  ;; because we're not inheriting from normal-mode, we need to
  ;; explicitly run file variables if the user wants to
  (condition-case err
      (hack-local-variables)
    (error (message "File local-variables error: %s"
                    (prin1-to-string err))))
  ;; check to see if the mode changed
  (when (eq major-mode 'planner-mode)
    (let ((hook (if (boundp 'write-file-functions)
                    'write-file-functions
                  'write-file-hooks)))
      (add-hook hook 'planner-renumber-notes-maybe t t)
      (add-hook hook 'planner-sort-tasks-maybe t t)
      (add-hook hook 'planner-renumber-tasks-maybe t t)
      (add-hook hook 'planner-align-tasks-maybe t t))
    (planner-setup-highlighting)
    (when (fboundp 'easy-menu-add)
      (easy-menu-add planner-menu planner-mode-map))
    (planner-prepare-file)))

(defun planner-setup-highlighting ()
  "Set up fontification for planner."
  (add-hook 'emacs-wiki-before-highlight-buffer-hook 'planner-zap-overlays t t)
  (add-hook 'emacs-wiki-before-highlight-buffer-hook 'planner-highlight-tasks t t)
  (add-hook 'emacs-wiki-before-highlight-buffer-hook 'planner-highlight-notes t t)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'planner-unhighlight-region)
  (set (make-local-variable 'font-lock-defaults)
       `(nil t nil nil 'beginning-of-line
	 (font-lock-fontify-region-function . emacs-wiki-highlight-region)
	 (font-lock-unfontify-region-function
	  . planner-unhighlight-region))))

;;;_ + Wiki pages

(defun planner-strip-whitespace (string)
  "Remove all whitespace from STRING.  Return the modified string."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[\r\n\t ]+" nil t)
      (replace-match ""))
    (buffer-string)))

(defun planner-page-default-p (&optional buffer)
  "Return t if this plan page can be safely deleted.
If the contents of this plan page are the same as the value of
`planner-day-page-template' or the plan page is empty, then no
information has been added and the page can safely be removed.

If BUFFER is given, considers the planner page in BUFFER instead.

Override this if `planner-day-page-template' is a function
instead of a string."
  (with-current-buffer (or buffer (current-buffer))
    (when (and (stringp planner-day-page-template)
               (not (> (buffer-size)
                       (+ (length planner-day-page-template)
                          planner-template-fuzz-factor))))
      (let ((body (planner-strip-whitespace (buffer-string))))
        (or (= (length body) 0)
            (string= body (planner-strip-whitespace
                           planner-day-page-template)))))))

(defvar planner-delete-file-function 'delete-file
  "Function called to remove a planner file from the current wiki.")

(defun planner-maybe-remove-file ()
  "Delete the planner file if it does not contain new information."
  (if (planner-page-default-p (current-buffer))
      (let ((filename buffer-file-name))
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))
        (when (file-exists-p filename)
          (funcall planner-delete-file-function filename)))
    (kill-buffer (current-buffer))))

(defvar planner-date-regexp
  "\\`\\([1-9][0-9][0-9][0-9]\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\'")

(defun planner-prepare-file ()
  "Insert some standard sections into an empty planner file."
  (when (= (buffer-size) 0)
    (let ((template
           (if (and (planner-page-name)
                    (string-match planner-date-regexp (planner-page-name)))
               planner-day-page-template
             planner-plan-page-template)))
      (if (functionp template)
          (funcall template)
        (insert template))
    (set-buffer-modified-p nil))))

(defun planner-browse-url-info (url)
  "If this in an Info URL, jump to it."
  (require 'info)
  (cond
   ((string-match "^info://\\([^#]+\\)#\\(.+\\)" url)
    (Info-find-node (match-string 1 url)
                    (match-string 2 url)))
   ((string-match "^info://\\([^#]+\\)" url)
    (Info-find-node (match-string 1 url)
                    "Top"))
   ((string-match "^info://(\\([^)]+\\))\\(.+\\)" url)
    (Info-find-node (match-string 1 url) (match-string 2 url)))
   ((string-match "^info://\\(.+\\)" url)
    (Info-find-node (match-string 1 url) "Top"))))

(defun planner-browse-url-man (url)
  "If this in a manpage URL, jump to it."
  (cond ((string-match "^man://\\(.+\\):\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url)
                               "(" (match-string 2 url) ")")))
        ((string-match "^man://\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url))))))

(defun planner-browse-url-google (url)
  "If this is a Google URL, jump to it."
  (let ((google-url (planner-resolve-url-google url)))
    (when google-url
      (browse-url google-url))))

(defun planner-resolve-url-google (url)
  "Return the correct Google search string."
  (when (string-match "^google:/?/?\\(.+\\)" url)
    (concat "http://www.google.com/search?q="
            (match-string 1 url))))

(defalias 'planner-set-sym-and-url-regexp 'emacs-wiki-set-sym-and-url-regexp)

(defun planner-update-wiki-project ()
  "Update the \"planner\" project in emacs-wiki-projects."
  ;; Remove the entry associated with Planner
  (setq emacs-wiki-projects
        (delq (assoc planner-project emacs-wiki-projects)
              emacs-wiki-projects))
  ;; Remove any entries that use the default Planner project name
  (setq emacs-wiki-projects
        (delq (assoc planner-project-default-name emacs-wiki-projects)
              emacs-wiki-projects))
  ;; Assign new contents to Planner entry
  (let ((new-regexp
         (if (and planner-name-regexp
                  (not (string-match (regexp-quote planner-name-regexp)
                                     emacs-wiki-name-regexp)))
             (concat emacs-wiki-name-regexp "\\|" planner-name-regexp)
           emacs-wiki-name-regexp)))
    (add-to-list
     'emacs-wiki-projects
     `(,planner-project
       . ((emacs-wiki-directories . (,planner-directory))
          (emacs-wiki-major-mode  . planner-mode)
          (emacs-wiki-markup-tags
           . ,(append planner-markup-tags
                      emacs-wiki-markup-tags))
          (emacs-wiki-publishing-markup
           . ,(append planner-publishing-markup
                      emacs-wiki-publishing-markup))
          (emacs-wiki-url-or-name-regexp . nil)
          (emacs-wiki-name-regexp
           . ,new-regexp)
          ;; this is here just so the right
          ;; url-or-name-regexp value is used
          (emacs-wiki-highlight-regexp . nil)
          (emacs-wiki-highlight-vector . nil)
          (emacs-wiki-highlight-markup
           . ,emacs-wiki-highlight-markup)
          (emacs-wiki-publishing-directory
           . ,planner-publishing-directory)
          ,@planner-custom-variables)))
    (emacs-wiki-update-project-interwikis)))

(defun planner-add-protocol (protocol browse-function resolve-function)
  "Add PROTOCOL to `emacs-wiki-url-protocols'.
BROWSE-FUNCTION should be a function that visits a URL in the
current buffer. RESOLVE-FUNCTION should be a function that
transforms a URL for publishing or returns nil if not linked."
  (add-to-list 'emacs-wiki-url-protocols
               (list protocol browse-function resolve-function))
  (emacs-wiki-set-sym-and-url-regexp 'emacs-wiki-url-protocols
                                     emacs-wiki-url-protocols))

(defalias 'planner-page-exists-p 'planner-page-file)

(defun planner-local-page-p (link)
  "Return non-nil if LINK seems to belong to the current wiki."
  (and link
       (not (or (string-match ":\\|/"
                              (planner-link-base link))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun planner-list-daily-files (&optional exclude-temp)
  "Return an unsorted list of daily files.
If EXCLUDE-TEMP is non-nil, ignore unsaved buffers."
  ;; get a list of all files
  ;; (save-some-buffers t (lambda () (equal 'planner-mode major-mode)))
  (let ((buffers (buffer-list))
        files)
    (mapcar (lambda (item)
              (when (string-match planner-date-regexp (car item))
                (setq files (cons (car item) files))))
            (planner-file-alist))
    (unless exclude-temp
      (while buffers
        (with-current-buffer (car buffers)
          (when (and (equal 'planner-mode major-mode)
                     (planner-page-name)
                     (string-match planner-date-regexp (planner-page-name)))
            (add-to-list 'files (planner-page-name))))
        (setq buffers (cdr buffers))))
    files))

(defun planner-get-day-pages (&optional from to exclude-temp)
  "Return a descending list of day pages from FROM to TO (inclusive).
If EXCLUDE-TEMP is non-nil, ignore unsaved pages."
  (sort (delq nil
              (mapcar
               (lambda (item)
                 (and
                  item
                  (string-match planner-date-regexp item)
                  (or (not from)
                      (string-lessp from item)
                      (equal from item))
                  (or (not to)
                      (string-lessp item to)
                      (equal item to))
                  item))
               (planner-list-daily-files exclude-temp)))
        (lambda (l r)
          (string-lessp r l))))

;;;_ + Date

(defvar planner-calendar-selected-date nil
  "Temporary storage for date selected from calendar.")

(defvar planner-use-calendar-flag t
  "*If non-nil, show calendar popup when reading a date.")

(defun planner-read-date (&optional prompt)
  "Prompt for a date string in the minibuffer.
If PROMPT is non-nil, display it as the prompt string."
  (save-window-excursion
    (when planner-use-day-pages
      (let ((old-buffer (current-buffer)))
        (when planner-use-calendar-flag (calendar))
        (let ((old-map (copy-keymap calendar-mode-map)))
          (unwind-protect
              (progn
                (define-key calendar-mode-map [return] 'planner-calendar-select)
                (define-key calendar-mode-map [mouse-1] 'planner-calendar-select)
                (setq planner-calendar-selected-date nil)
                (let ((text (read-string
                             (format "%s %s"
                                     (or prompt "When")
                                     (format-time-string
                                      "(%Y.%m.%d, %m.%d, %d): ")))))
                  (or planner-calendar-selected-date
                      (with-current-buffer old-buffer (planner-expand-name text)))))
            (setq calendar-mode-map old-map)))))))

(defvar planner-timewarp-date nil
 "*Date to timewarp to for planner.
Should be a string of the form YYYY.MM.DD. If nil, do not timewarp.")

;; This should be handy for remembering lots of notes onto particular days.
(defun planner-timewarp (date)
 "Timewarp to DATE."
 (interactive (list (let ((planner-timewarp-date nil)) (planner-read-date))))
 (setq planner-timewarp-date date)
 (if date (message "Timewarped to %s" date)
   (message "Timewarped back to the present")))

(defun planner-today ()
  "Return the filename of the current date."
  (if planner-use-day-pages
      (or planner-timewarp-date (planner-date-to-filename (decode-time (current-time))))
    "WelcomePage"))

(defun planner-date-to-filename (date)
  "Return the planner filename corresponding to DATE.
DATE is a list (month day year) or an internal date representation."
  (if (= (length date) 3)
      (format "%04d.%02d.%02d" (elt date 2) (elt date 0) (elt date 1))
    (if (= (length date) 2)
        (setq date (decode-time date)))
    (format "%04d.%02d.%02d"
            (elt date 5) ; year
            (elt date 4) ; month
            (elt date 3)))) ; day

(defun planner-calculate-date-from-day-offset (origin offset)
  "From ORIGIN, calculate the date OFFSET days into the past or future.
ORIGIN can be a buffer name, a list of the form (MONTH DAY YEAR),
or an internal date representation.  If OFFSET is positive,
returns a date in the future.  If OFFSET is negative, returns the
date -OFFSET days in the past.  Return an object that is the
same type as ORIGIN."
  (cond
   ((stringp origin)
    (let ((date (planner-filename-to-calendar-date origin)))
      (planner-date-to-filename (encode-time 0 0 0 (+ (elt date 1) offset)
                                             (elt date 0) (elt date 2)))))
   ((= (length origin) 2)
    (encode-time 0 0 0 (+ (elt origin 1) offset)
                 (elt origin 0) (elt origin 2)))
   ((= (length origin) 3)
    (let ((result
           (decode-time (encode-time 0 0 0 (+ (elt origin 1) offset)
                                     (elt origin 0) (elt origin 2)))))
      (list (elt result 4) (elt result 3) (elt result 5))))))

(defun planner-get-previous-existing-day (date)
  "Return the planner file immediately before DATE.
DATE is a filename or a list (month day year).  When called from
a planner file, DATE defaults to the date of this file, otherwise
it defaults to today.  Returns an object of the same type as
DATE."
  (let ((newdate (if (listp date) (planner-date-to-filename date) date))
        (result nil))
    ;; beginning of hackish part
    (mapcar (lambda (elt)
              (when (and (or (not result) (not (or (string= elt result)
                                                   (string< elt result))))
                         (string< elt newdate))
                (setq result elt))) (planner-list-daily-files))
    (if result
        (if (listp date)
            (planner-filename-to-calendar-date result)
          result)
      (error "No previous planner file"))))

(defun planner-get-next-existing-day (date)
  "Return the existing planner file immediately after DATE.
DATE is a filename or a list (month day year).  When called from
a planner file, DATE defaults to the date of this file, otherwise
it defaults to today.  Returns an object of the same type as
DATE."
  (let ((newdate (if (listp date) (planner-date-to-filename date) date))
        (result nil))
    ;; beginning of hackish part
    (mapcar (lambda (elt)
              (when (and (or (not result)
                             (string< elt result))
                         (not (or (string= elt newdate)
                                  (string< elt newdate))))
                (setq result elt))) (planner-list-daily-files))
    (if result
        (if (listp date)
            (planner-filename-to-calendar-date result)
          result)
      (error "No next planner file"))))

(defun planner-yesterday ()
  "Return the date yesterday."
  (planner-calculate-date-from-day-offset (planner-today) -1))

(defcustom planner-expand-name-favor-future-p nil
  "If non-nil, `planner-expand-name' defaults to future dates."
  :type 'boolean
  :group 'planner)

(defcustom planner-expand-name-default "."
  "What an empty string means in `planner-expand-name'.
\".\" means today."
  :type '(choice
          (const :tag "Today" ".")
          (const :tag "None" nil)
          string)
  :group 'planner)

(defvar planner-expand-name-days-alist '(("sun" . 0)
                                         ("mon" . 1)
                                         ("tue" . 2)
                                         ("wed" . 3)
                                         ("thu" . 4)
                                         ("fri" . 5)
                                         ("sat" . 6))
  "Abbreviations for `planner-expand-name'.")

(defun planner-expand-name (name)
  "Expand the given NAME to its fullest form.
This typically means that dates like 3.31 will become 2001.03.31.
NOTE: This function no longer uses the current buffer filename for
defaults."
  (let ((now (planner-filename-to-calendar-date (planner-today)))
        name-year name-month name-day)
    (when (string-match "^\\s-*$" name)
      (setq name (or planner-expand-name-default "nil")))
    (cond
     ((string= "nil" name) nil)
     ((string= "." name) (planner-today))
     ((string-match (concat "^\\([1-9][0-9][0-9][0-9]\\.\\)?"
                            "\\(\\([0-9]+\\)\\.\\)?"
                            "\\([0-9]+\\)\\(#.*\\)?$") name)
      (setq name-year
            (if (match-string 1 name)
                (string-to-number (match-string 1 name)) (nth 2 now)))
      (setq name-month
            (if (match-string 3 name)
                (string-to-number (match-string 3 name)) (nth 0 now)))
      (setq name-day
            (if (match-string 4 name)
                (string-to-number (match-string 4 name)) (nth 1 now)))
      (when (and planner-expand-name-favor-future-p
                 (planner-time-less-p
                  (encode-time 59 59 23
                               name-day name-month name-year)
                  (current-time)))
        (cond
         ((match-string 1 name)) ; Do nothing if the year is specified
         ((match-string 2 name)
          (setq name-year (1+ name-year)))
         ((match-string 4 name)
          (setq name-month (1+ name-month)))))
      (planner-date-to-filename (encode-time 59 59 23
                                             name-day name-month name-year)))
     ((string-match "^\\([-+]\\)\\s-*\\([0-9]+\\)$" name)
      ;; Today + or - that number of days
      (planner-calculate-date-from-day-offset
       (if (or planner-dates-relative-to-today-flag
               (not (planner-page-name))
               (not (save-match-data (string-match planner-date-regexp (planner-page-name)))))
           (planner-today)
         (planner-page-name))
       (string-to-number
        (concat (match-string 1 name) (match-string 2 name)))))
     ((let ((case-fold-search nil))
        (string-match (concat
                       "^\\([-+]\\)\\s-*\\([0-9]*\\)\\s-*\\("
                       (mapconcat 'car planner-expand-name-days-alist "\\|")
                       "\\)\\s-*\\(\\.\\|\\(\\(\\([0-9]+\\.\\)?[0-9]+\\.\\)?[0-9]+\\)\\)?$")
                      name))
      (let* ((day (cdr (assoc (match-string 3 name)
                              planner-expand-name-days-alist)))
             (offset (string-to-number
                      (concat (match-string 1 name)
                              (if (and
                                   (match-string 2 name)
                                   (not (string= (match-string 2 name) "")))
                                  (match-string 2 name)
                                "1"))))
             (base-date (planner-filename-to-calendar-date
                         (if (and (match-string 4 name)
                                  (not (string= (match-string 4 name) "")))
                             (planner-expand-name (match-string 4 name))
                           (if (or planner-dates-relative-to-today-flag
                                   (not (planner-page-name))
                                   (not (save-match-data
                                          (string-match planner-date-regexp (planner-page-name)))))
                               (planner-today)
                             (planner-page-name))))))
        (planner-date-to-filename
         (calendar-gregorian-from-absolute
          (calendar-dayname-on-or-before
           day
           (+ (calendar-absolute-from-gregorian base-date)
              (* offset 7)
              (if (< offset 0) 6 0)))))))
     (t name))))

(defun planner-get-current-date-filename ()
  "Return the date of the daily page currently being viewed.
If no daily page is being viewed, return today's date."
  (if (string-match planner-date-regexp (planner-page-name))
      (planner-page-name)
    (planner-today)))

(defun planner-filename-to-calendar-date (filename)
  "Return the date of the planning file FILENAME.
Date is a list (month day year)."
  (unless (string-match planner-date-regexp filename)
    (error "Not convertible to a date %s" filename))
  (list (string-to-number (substring filename 5 7)) ; month
        (string-to-number (substring filename 8 10)) ; day
        (string-to-number (substring filename 0 4)))) ; year

;;;_ + Sections

(defun planner-narrow-to-section (section &optional create)
  "Widen to the whole page and narrow to the section labelled SECTION.
If CREATE is non-nil, create the section if it is not found.
Return non-nil if SECTION was found."
  (interactive "MSection: ")
  (widen)
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (goto-char (point-min))
  (when (or
         (re-search-forward
          (concat "^*\\s-+" (regexp-quote section) "\\s-*$") nil t)
         (and create
              (funcall planner-create-section-function section)
              (goto-char (point-min))
              (re-search-forward (concat "^*\\s-+" (regexp-quote section) "\\s-*$") nil t)))
    (let ((beg (match-beginning 0))
          (end (if (re-search-forward "^*\\s-+" nil t)
                   (match-beginning 0) (point-max))))
      (narrow-to-region beg end)
      t)))

(defun planner-delete-section (section)
  "Delete the named SECTION."
  (unless (planner-derived-mode-p 'planner-mode)
    (error "This is not a planner buffer"))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\*\\s-+" section "\\(\\s-*\\)$") nil t)
    (let ((beg (planner-line-beginning-position))
          (end (if (re-search-forward "^* " nil t)
                   (planner-line-beginning-position)
                 (point-max))))
      (delete-region beg end))))

(defun planner-delete-section-text (section)
  "Delete the text of the named SECTION."
  (unless (planner-derived-mode-p 'planner-mode)
    (error "This is not a planner buffer"))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\*\\s-+" section "\\(\\s-*\\)$") nil t)
    (let ((beg (point))
          (end (if (re-search-forward "^* " nil t)
                   (planner-line-beginning-position)
                 (point-max))))
      (delete-region beg end)
      (goto-char (planner-line-beginning-position)))))

(defun planner-seek-to-first (&optional section)
  "Positions the point at the specified SECTION, or Tasks if not specified."
  (interactive)
  (unless section
    (setq section planner-default-section))
  (unless (stringp section)
    (setq section (cdr (assoc section planner-sections))))
  (widen)
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\*\\s-+" section "\\(\\s-*?\\)$") nil t)
      (let ((old (point)) new)
        (forward-line 1)
        (if (re-search-forward "[^\\s-]" nil t)
            (progn
              (goto-char (planner-line-beginning-position))
              (unless (looking-at "^\\*\\s-")
                (setq new (point)))))
        (goto-char (or new old))
        (unless new
          (forward-line 1)
          (when (or (looking-at "^\\*\\s-+")
                    (> (forward-line 1) 0)) (insert "\n"))
          (when (or (looking-at "^\\*\\s-+")
                    (> (forward-line 1) 0)) (insert "\n"))
          (when (looking-at "^\\*\\s-+") (forward-line -1))))
    ;; Section not found, so create it.
    (funcall planner-create-section-function section)))

(defun planner-create-at-top (section)
  "Create SECTION at top of file."
  (goto-char (point-min))
  (let ((buffer-status (buffer-modified-p)))
    (insert "* " section "\n\n")
    (set-buffer-modified-p buffer-status)))

(defun planner-create-at-bottom (section)
  "Create SECTION at bottom of file."
  (goto-char (point-max))
  (let ((buffer-status (buffer-modified-p)))
    (insert "\n* " section "\n\n")
    (set-buffer-modified-p buffer-status)))

;;;_ + Basic annotation

;;;###autoload
(defun planner-annotation-as-kill (arg)
  "Copy the current annotation into the kill ring.
When called with a prefix argument, prompt for the link display name."
  (interactive "P")
  (let* ((link (run-hook-with-args-until-success 'planner-annotation-functions))
         (link-name (if arg (read-string (format "Link name for %s: " link)))))
    (unless (= 0 (length link-name))
      (setq link (planner-make-link link link-name t)))
    (message "Copied '%s' to the kill-ring." link)
    (kill-new link)))

(defun planner-annotation-from-planner-note ()
  "Return a link to the current page.
Call when the point is on the first line of the note."
  (when (and (planner-derived-mode-p 'planner-mode)
             (planner-page-name))
    (save-excursion
      (goto-char (planner-line-beginning-position))
      (when (looking-at ".\\(#[0-9]+\\)")
        (planner-make-link
         (concat (planner-page-name)
                 (planner-match-string-no-properties 1))
         (concat (planner-page-name)
                 (planner-match-string-no-properties 1))
         t)))))

(defun planner-annotation-from-planner ()
  "Return a wiki link to the current wiki page.
Date pages are not linked."
  (when (and (planner-derived-mode-p 'planner-mode)
             (planner-page-name))
    (cond
     ((string-match planner-date-regexp (planner-page-name))
      "") ; None for date pages
     (t (planner-make-link (planner-page-name) nil t)))))

(defun planner-annotation-from-wiki ()
  "Return the interwiki link to the current wiki page."
  (when (and (planner-derived-mode-p 'emacs-wiki-mode)
             emacs-wiki-current-project
             (emacs-wiki-page-name))
    (concat "[[" emacs-wiki-current-project "#" (emacs-wiki-page-name) "]]")))

(defun planner-annotation-from-dired ()
  "Return the `default-directory' of the current Dired buffer."
  (when (eq major-mode 'dired-mode)
    (planner-make-link default-directory)))

(defun planner-annotation-from-file-relative ()
  "Return the filename of the current buffer relative to `planner-directory'."
  (when buffer-file-name
    (planner-make-link (file-relative-name buffer-file-name (planner-directory)) nil t)))

(defcustom planner-annotation-use-relative-file nil
  "If t, use relative file links always.
If a function, it is called with the file name. Return value of t
means use relative file links."
  :group 'planner
  :type '(choice (const :tag "Always use relative file links" t)
                 (const :tag "Never use relative file links" nil)
                 function))

(defcustom planner-annotation-strip-directory nil
  "If non-nil, strip the directory part of the filename from link text."
  :group 'planner
  :type 'boolean)

(defcustom planner-annotation-format-local-file-name nil
  "If non-nil, use the result of `planner-annotation-format-local-file-name'."
  :group 'planner
  :type '(choice (const :tag "Use filename as is" nil)
                 function))

(defun planner-annotation-from-file ()
  "Return the filename of the current buffer.
If `planner-annotation-use-relative-file' is t or a function that
returns non-nil, a relative link is used instead. If
`planner-annotation-strip-directory' is non-nil, the directory is
stripped from the link description."
  (when buffer-file-name
    (planner-make-link
     (if (or (and (functionp planner-annotation-use-relative-file)
                  (funcall planner-annotation-use-relative-file
                           (buffer-file-name)))
             (equal planner-annotation-use-relative-file t))
         (file-relative-name (buffer-file-name) (planner-directory))
       (if (functionp planner-annotation-format-local-file-name)
           (funcall planner-annotation-format-local-file-name buffer-file-name)
         buffer-file-name))
     (when planner-annotation-strip-directory
       (file-name-nondirectory buffer-file-name))
     t)))

;;;###autoload
(defun planner-annotation-from-file-with-position ()
  "Return the filename and cursor position of the current buffer.
If `planner-annotation-use-relative-file' is t or a function that
returns non-nil, a relative link is used instead. If
`planner-annotation-strip-directory' is non-nil, the directory is
stripped from the link description."
  (when buffer-file-name
    (planner-make-link
     (concat
      "pos://"
      (if (or (and (functionp planner-annotation-use-relative-file)
                   (funcall planner-annotation-use-relative-file
                            (buffer-file-name)))
              (equal planner-annotation-use-relative-file t))
          (file-relative-name (buffer-file-name) (planner-directory))
        buffer-file-name)
      "#" (number-to-string (point)))
     (if planner-annotation-strip-directory
         (file-name-nondirectory buffer-file-name)
       buffer-file-name)
     t)))

;;;###autoload
(defun planner-browse-position-url (url)
  "If this is a position URL, jump to it."
  (when (string-match "^pos://\\(.+\\)#\\([0-9]+\\)$" url)
    (let ((file (match-string 1 url))
          (pos (string-to-number (match-string 2 url))))
    (find-file file)
    (goto-char pos)
    t)))

;;;###autoload
(defun planner-resolve-position-url (id)
  "Replace ID with the blog, web or e-mail address of the BBDB record."
  (save-match-data
    (when (string-match "^pos://\\(.+\\)#\\([0-9]+\\)" id)
      (match-string 1 id))))

(planner-add-protocol "pos" 'planner-browse-position-url 'planner-resolve-position-url)
(custom-add-option 'planner-annotation-functions 'planner-annotation-from-file-with-position)

;;;_ + Tasks

(defcustom planner-create-task-hook nil
  "Functions to run after a task has been created.
Point will be on the same line as the task."
  :type 'hook
  :group 'planner-tasks)

(defcustom planner-create-task-from-buffer-hook nil
  "Functions to run after a task has been created from a buffer.
This will be run before `planner-create-task-hook'.
Point will be on the same line as the task."
  :type 'hook
  :group 'planner-tasks)

(defcustom planner-task-dates-favor-future-p nil
  "*If this is non-nil, favor future dates for task creation or movement."
  :type 'boolean
  :group 'planner-tasks)

(defcustom planner-default-page "TaskPool"
  "Default page for tasks.
This is set to the current planner page, or the last page used
if not on a plan page."
  :type 'string
  :group 'planner-tasks)

(defcustom planner-tasks-file-behavior 'close
  "Controls behavior of task creation and updates.
If 'close, newly-opened files are saved and closed.
If 'save, newly-opened files are saved and left open.
If nil, no actions will be taken."
  :group 'planner-tasks
  :type '(choice (const :tag "Save and close opened files" 'close)
                 (const :tag "Save opened files" 'save)
                 (const :tag "Do nothing" nil)))

(defcustom planner-tasks-never-suppress-fixing-flag t
  "Non-nil means always sort, renumber and align tasks whenever files are saved."
  :group 'planner-tasks
  :type 'boolean)

(defcustom planner-sort-undated-tasks-equivalent "9999.99.99"
  "Date considered for undated tasks.
This option controls task sorting on plan pages.  By default,
undated tasks are sorted after dated tasks."
  :group 'planner-tasks
  :type
  '(choice
    (const :tag "Sort undated tasks after dated tasks" "9999.99.99")
    (const :tag "Sort undated tasks before dated tasks" "")
    string))

(defcustom planner-sort-tasks-key-function 'planner-sort-tasks-default-key
  "Function called to determine the sorting key for the current line."
  :group 'planner-tasks
  :type 'function)

(defcustom planner-use-task-numbers nil
  "Non-nil means number tasks.
This allows you to refer to past tasks if your tasks are numbered
appropriately.  If you set this to nil, you can save space in your
plan files."
  :type 'boolean
  :group 'planner-tasks)

;;;_   + Information

(defun planner-task-info-from-string (page-name string)
  "On the planner page PAGE-NAME, parse STRING and return the task as a list.
Argument PAGE-NAME is used to determine whether this is a link
from a plan page or a date page."
  (with-planner
   (when (string-match "#\\([A-C]\\)\\([0-9]*\\)\\s-+\\(.\\)\\s-+\\(.+\\)" string)
     (let ((priority (planner-match-string-no-properties 1 string))
           (number (planner-match-string-no-properties 2 string))
           (status (planner-match-string-no-properties 3 string))
           (description (planner-match-string-no-properties 4 string))
           (case-fold-search nil)
           link-text link plan date)
       (when (= (length number) 0)
         (setq number nil))
       (cond
        ((string-match
          "\\s-+(\\(\\[\\[\\([^])]+\\)\\]\\[\\([^])]+\\)\\]\\]\\))\\s-*$"
          description)
         (setq link-text (match-string 1 description))
         (setq link (match-string 2 description))
         (setq description (replace-match "" t t description)))
        ((string-match
          "\\s-+(\\(\\[\\[\\([^])]+\\)\\]\\]\\))\\s-*$" description)
         (setq link-text (match-string 1 description))
         (setq link (match-string 2 description))
         (setq description (replace-match "" t t description)))
        ((string-match "\\s-+(\\([^)]+\\))\\s-*$" description)
         (setq link-text (match-string 1 description))
         (setq link (match-string 1 description))
         (setq description (replace-match "" t t description)))
        ((string-match "\\s-+$" description)
         (setq description (replace-match "" t t description))))
       (when link
         (setq link (planner-link-base link-text)))
       (unless (planner-local-page-p link) (setq link nil))
       (if (string-match planner-date-regexp page-name)
           ;; We're on a date page, so the link page (if any) should be the
           ;; planner page.
           (progn
             (setq date page-name)
             (setq plan (and link
                             (unless (string-match planner-date-regexp link)
                               link))))
         ;; We're on a planner page, so the link page (if any) will be the plan
         (setq plan (and page-name (unless (string-match planner-date-regexp
                                                         page-name) page-name)))
         (setq date link))
       (list page-name
             priority number status description link link-text plan date)))))

(defun planner-task-info-override (task-info properties)
  "Replace fields in TASK-INFO with PROPERTIES.
Acceptable properties are: page-name priority number status
description link link-text plan date."
  (let ((fields '(page-name priority number status description
                            link link-text plan date))
        result)
    (while task-info
      (setq result
            (cons
             (car (let ((search (memq (car fields) properties)))
                    (if search (cdr search) task-info)))
             result))
      (setq fields (cdr fields))
      (setq task-info (cdr task-info)))
    (nreverse result)))

(defun planner-current-task-info ()
  "Parse the current line and return the task information as a list."
  (planner-task-info-from-string (planner-page-name)
                                 (buffer-substring
                                  (planner-line-beginning-position)
                                  (planner-line-end-position))))

(defun planner-task-page (info)
  "Return the page of a task given INFO." (nth 0 info))
(defun planner-task-priority (info)
  "Return the priority of a task given INFO." (nth 1 info))
(defun planner-task-number (info)
  "Return the number of a task given INFO." (nth 2 info))
(defun planner-task-status (info)
  "Return the status of a task given INFO." (nth 3 info))
(defun planner-task-description (info)
  "Return the description of a task given INFO." (nth 4 info))
(defun planner-task-link (info)
  "Return the page linked to by a task given INFO." (nth 5 info))
(defun planner-task-link-text (info)
  "Return the link text of a task given INFO." (nth 6 info))
(defun planner-task-plan (info)
  "Return the planner page of a task given INFO." (nth 7 info))
(defun planner-task-date (info)
  "Return the planner date of a task given INFO." (nth 8 info))

;;;_   + Creation

(defvar planner-create-task-from-info-function 'planner-create-task-from-info-basic
  "Function for creating tasks.
Should accept the same arguments as `planner-create-task-from-info-basic'.")

(defun planner-create-task-from-info (info &optional priority number status description link-text date plan)
  "Create a task in the date and plan pages based on INFO.
Optional arguments PRIORITY, NUMBER, STATUS, DESCRIPTION,
LINK-TEXT, DATE, and PLAN override those in INFO."
  (funcall planner-create-task-from-info-function info priority number status description link-text date plan))

(defun planner-create-task-from-info-basic
  (info &optional priority number status description link-text date plan)
  "Create a task in the date and plan pages based on INFO.
Optional arguments PRIORITY, NUMBER, STATUS, DESCRIPTION,
LINK-TEXT, DATE, and PLAN override those in INFO."
  (save-window-excursion
    (save-excursion
      ;; page-name priority number status description
      ;; link link-text plan date
      ;; Create the task in the plan page
      (let ((plan-page (or plan (planner-task-plan info)))
            (date-page (or date (planner-task-date info)))
            (live-buffers
             (and (equal planner-tasks-file-behavior 'close)
                  (buffer-list))))
        (when plan-page
          (if (string-match planner-date-regexp
                            plan-page)
              (setq plan-page nil)))
        (when (and plan-page (not (string= plan-page "")))
          (planner-find-file plan-page)
          (planner-seek-task-creation-point)
          (insert (planner-format-task info priority number
                                       status description
                                       (planner-make-link date-page)
				       (planner-make-link date-page))
                  "\n"))
        ;; Create the task in the date page
        (when (and date-page (not (string= date-page "")))
          (planner-goto date-page)
          (planner-seek-task-creation-point)
          (insert (planner-format-task info priority number
                                       status description
                                       (or
                                        link-text
                                        (planner-task-link-text info))
                                       plan-page) "\n"))
        (forward-line -1)
        (run-hooks 'planner-create-task-hook)
        (when planner-tasks-file-behavior
          (planner-save-buffers live-buffers t))))))

(defvar planner-task-format "#%s%-2s %s %s%s"
  "Format used by `planner-format-task' when inserting new tasks.")

(defun planner-format-task
  (task-info &optional priority number status description link-text link)
  "Return a string containing TASK-INFO ready to be inserted into a page.
Non-nil values of PRIORITY, NUMBER, STATUS, DESCRIPTION, LINK-TEXT,
and LINK override TASK-INFO."
  (format planner-task-format
          (or priority (planner-task-priority task-info))
          (if planner-use-task-numbers
              (or number (planner-task-number task-info) "")
            "")
          (or status (planner-task-status task-info))
          (or description (planner-task-description task-info))
          (let ((text (or link-text
                          (and link (planner-make-link link))
                          (planner-task-link-text task-info))))
            (if (and text (not (equal text "")))
                (concat " ("
                        text
                        ")")
              ""))))

;;;_   + Scheduling

(defun planner-copy-or-move-region (beg end &optional date muffle-errors)
  "Move all tasks from BEG to END to DATE.
If this is the original task, it copies it instead of moving.
Most of the time, the original should be kept in a planning file,
but this is not required.  `planner-copy-or-move-region' will
copy or move all tasks from the line containing BEG to the line
just before END.  If MUFFLE-ERRORS is non-nil, no errors will be
reported."
  (interactive "r")
  (unless date (setq date
                     (let ((planner-expand-name-favor-future-p
                            (or planner-expand-name-favor-future-p
                                planner-task-dates-favor-future-p)))
                       (planner-read-date))))
  (let ((start (if (< beg end) beg end))
        (finish (if (< beg end) end beg))
        (buffer (current-buffer))
        (error-count 0)
        (count 0)
        (live-buffers (when (equal planner-tasks-file-behavior
                                   'close)
                        (buffer-list)))
        done)
    ;; Invoke planner-copy-or-move-task on each line in reverse
    (let ((planner-tasks-file-behavior nil))
      (save-excursion
        (save-restriction
          (narrow-to-region
           (and (goto-char start) (planner-line-beginning-position))
           (and (goto-char (1- finish))
                (min (point-max)
                     (1+ (planner-line-end-position)))))
          (when planner-add-task-at-end-flag
            (reverse-region (point-min) (point-max)))
          (goto-char (point-max))
          (while (not done)
            (goto-char (planner-line-beginning-position))
            ;; Non-completed or cancelled tasks only
            (when (looking-at
                   "^#?\\([A-C]\\)\\([0-9]*\\)\\s-+\\([^XC]\\)\\s-+\\(.+\\)")
              (condition-case err
                  (when (planner-copy-or-move-task date)
                    (setq count (1+ count)))
                (error
                 (unless (or muffle-errors (not (interactive-p)))
                   (message
                    "Error with %s: %s"
                    (elt (planner-current-task-info) 4) err)
                   (setq error-count (1+ error-count)))
                 nil)))
            (when (bobp)
              (setq done t))
            (forward-line -1))
          (when planner-add-task-at-end-flag
            (reverse-region (point-min) (point-max)))
          (when (and (not muffle-errors)
                     (not error-count)
                     (> error-count 0)
                     (interactive-p))
            (message (if (> error-count 1) "%d errors." "%d error.")
                     error-count)))))
    (when planner-tasks-file-behavior
      (planner-save-buffers live-buffers))
    (set-buffer buffer)
    count))  ; Return the number of tasks moved.

;;;_   + Navigation

(defvar planner-jump-to-linked-task-function 'planner-jump-to-linked-task-basic
  "Function to jump to a linked task. Function should have one optional parameter: TASK-INFO.")

(defun planner-jump-to-linked-task (&optional task-info)
  "Display the task page linked to by the current task or TASK-INFO."
  (funcall planner-jump-to-linked-task-function task-info))

(defun planner-jump-to-linked-task-basic (&optional task-info)
  "Display the task page linked to by the current task or TASK-INFO."
  (interactive)
  (let* ((task-info (or task-info (planner-current-task-info)))
         (link (and task-info (planner-task-link task-info))))
    (when (planner-local-page-p link)
      (planner-find-file (planner-task-link task-info))
      (widen)
      (goto-char (point-min))
      (when (search-forward (planner-task-description task-info) nil t)
        (beginning-of-line)
        t))))

;;;_   + Convenience

(defvar planner-history-list nil "History list for pages.")

(defvar planner-read-name-function 'planner-read-name-single
  "Function to call in order to read the names of pages.")

(defun planner-read-name (file-alist &optional prompt initial)
  "Read the name of a valid Wiki page from minibuffer.
FILE-ALIST is a list of (page-name . filename) entries. If PROMPT
is non-nil, it is used as the prompt string. If INITIAL is specified,
it is used as a reasonable default."
  (funcall planner-read-name-function file-alist prompt initial))

(defun planner-read-name-single (file-alist &optional prompt initial)
  "Read the name of a valid Wiki page from minibuffer with completion.
FILE-ALIST is a list of (page-name . filename) entries. If PROMPT
is non-nil, it is used as the prompt string. If INITIAL is specified,
it is used as a reasonable default."
  (let* ((default planner-default-page)
         (str (completing-read
               (format "%s(default: %s) " (or prompt "Page: ") default)
               file-alist nil nil initial 'planner-history-list)))
    (cond
     ((or (null str) (= (length str) 0)) default)
     ((string= str "nil") nil)
     (t str))))

(defun planner-read-name-no-completion (names &optional prompt initial)
  "Read the name of a valid Wiki page from minibuffer without completion.
FILE-ALIST is a list of (page-name . filename) entries. If PROMPT
is non-nil, it is used as the prompt string. If INITIAL is specified,
it is used as a reasonable default."
  (let* ((default planner-default-page)
         (str (read-string
               (format "%s(default: %s) " (or prompt "Page: ") default)
               initial 'planner-history-list default)))
        (cond
         ((or (null str) (= (length str) 0)) default)
         ((string= str "nil") nil)
         (t str))))

(defun planner-read-non-date-page (file-alist &optional prompt initial)
  "Prompt for a page name that does not match `planner-date-regexp'.
Base completion on FILE-ALIST. If PROMPT is non-nil, use that as
the prompt. If INITIAL is non-nil, use that as the initial contents
of the minibuffer."
  (planner-read-name
   (delq nil
    (mapcar
     (lambda (item)
       (unless (string-match
                (concat "^\\(?:" planner-date-regexp "\\)$")
                (car item))
         item))
     (copy-alist file-alist)))
   prompt initial))

(defvar planner-find-task-function 'planner-find-task-basic
  "Function to find a task based on INFO and POINT.")

(defun planner-find-task (info &optional point)
  "Move point to the character before the task described by INFO.
If POINT is specified, start search from that point."
  (funcall planner-find-task-function info point))

(defun planner-find-task-basic (info &optional point)
  "Move point to the character before the task described by INFO.
If POINT is specified, start search from that point."
  (goto-char (or point (point-min)))
  (when (re-search-forward
         (concat
          "^#[A-C][0-9]*\\s-+.\\s-+"
          (regexp-quote (planner-task-description info))) nil t)
    (goto-char (planner-line-beginning-position))))

(defun planner-tasks-equal-p (task-a task-b)
  "Return t if TASK-A and TASK-B are equivalent.
This is true if they have the same value for priority, status,
description, plan and date."
  (and (string= (or (planner-task-priority task-a) "")
                (or (planner-task-priority task-b) ""))
       (string= (or (planner-task-status task-a) "")
                (or (planner-task-status task-b) ""))
       (string= (or (planner-task-description task-a) "")
                (or (planner-task-description task-b) ""))
       (string= (or (planner-task-plan task-a) "")
                (or (planner-task-plan task-b) ""))
       (string= (or (planner-task-date task-a) "")
                (or (planner-task-date task-b) ""))))

(defun planner-save-buffers (&optional buffer-list suppress-fixing skip-buffer)
  "Save all planner buffers.
If BUFFER-LIST is a list of buffers, close all buffers not found
in that list. If SUPPRESS-FIXING is non-nil, do not perform any
planner-related modifications such as task sorting. If
SKIP-BUFFER is non-nil, do not save that buffer."
  (interactive)
  (setq suppress-fixing (and (not planner-tasks-never-suppress-fixing-flag)
                             suppress-fixing))
  (mapcar
   (lambda (buffer)
     (unless (eq buffer skip-buffer)
       (with-current-buffer buffer
         ;; Save all planner buffers
         (when (and (planner-derived-mode-p 'planner-mode)
                    buffer-file-name
                    (planner-page-name)
                    (not (string= "" (planner-page-name))))
           ;; SUPPRESS-FIXING is negated in the following forms because
           ;; it makes more sense to let planner-save-buffers do the
           ;; usual actions when the parameter is omitted.
           (let ((planner-sort-tasks-automatically
                  (and planner-sort-tasks-automatically
                       (not suppress-fixing)))
                 (planner-renumber-tasks-automatically
                  (and planner-renumber-tasks-automatically
                       (not suppress-fixing)))
                 (planner-align-tasks-automatically
                  (and planner-align-tasks-automatically
                       (not suppress-fixing)))
                 (planner-renumber-notes-automatically
                  (and planner-renumber-notes-automatically
                       (not suppress-fixing)))
                 (planner-tasks-save-behavior nil)
                 (planner-id-update-automatically nil))
             (when (buffer-modified-p)
               (save-buffer)))
           (when (and buffer-list
                      (not (memq buffer buffer-list)))
           (kill-buffer nil))))))
   (buffer-list)))

;;;_   + Extraction

(defvar planner-task-regexp
  (concat "^#[A-C][0-9]*\\s-+" planner-marks-regexp "\\s-+")
  "Regexp used to match tasks.")

(defvar planner-live-task-regexp "^#[ABC][0-9]*\\s-+[_o>P]\\s-+"
  "Regular expression matching \"live\" tasks.
A task is live if it is not finished and it is not cancelled.")

(defun planner-extract-tasks (pages &optional condition)
  "Parse PAGES and extract all tasks.
If CONDITION is non-nil, it should be a function that
accepts the task info as an argument and returns t if
the task should be added to the list."
  (with-temp-buffer
    (let ((list (planner-file-alist))
          result)
      (while pages
        (erase-buffer)
        (insert-file-contents (cdr (assoc (car pages) list)))
        (goto-char (point-max))
        (while (re-search-backward "^#[A-C]" nil t)
          (let ((info
                 (planner-task-info-from-string
                  (car pages)
                  (buffer-substring
                   (line-beginning-position)
                   (line-end-position)))))
            (when (and info
                       (if condition
                           (funcall condition info)
                         t))
              (setq result (append (list info) result)))))
        (setq pages (cdr pages)))
      result)))

(defun planner-extract-tasks-with-status (pages status)
  "Return all tasks on PAGES with the specified STATUS."
  (planner-extract-tasks pages
                         (lambda (item)
                           (equal (planner-task-status item)
                                  status))))

(defun planner-tasks-tag (beg end attrs)
  "Replace the region BEG to END with a report of tasks.
If status is specified in ATTRS, list tasks matching that status only.
To negate the sense of a match, use a regexp."
  (delete-region beg end)
  (let* ((status (cdr (assoc "status" attrs)))
         (tasks (planner-extract-tasks
                 (planner-get-day-pages nil nil t)
                 (if status
                     (lambda (item)
                       (string-match status (planner-task-status item)))
                   nil))))
    (while tasks
      (insert
       (planner-make-link (planner-task-page (car tasks)) nil t)
       " | "
       (planner-task-priority (car tasks))
       " | "
       (planner-task-status (car tasks))
       " | "
       (planner-task-description (car tasks))
       "\n")
      (setq tasks (cdr tasks)))))

(defvar planner-on-date-page nil
  "Internal variable used in `planner-sort-tasks-default-key'.")

(defun planner-sort-tasks-default-key ()
  "Provide old sorting behavior.
Day pages sort by status and priority. Plan pages sort by date,
status and priority."
  (if planner-on-date-page
      (planner-sort-tasks-basic)
    (planner-sort-tasks-by-date)))

(defun planner-sort-tasks-basic ()
  "Sort tasks by status (_P>XC) and priority (ABC)."
  (skip-chars-forward "#ABC")
  (let ((case-fold-search t)
        (ch (char-before))
        status)
    (skip-chars-forward "0123456789 ")
    (setq status (char-after))
    (+ ;(read (current-buffer))
     (cond
      ((eq status ?P) 1000)
      ((eq status ?>) 2000)
      ((eq status ?X) 3000)
      ((eq status ?C) 4000)
      (t 0))
     (cond ((eq ch ?A) 100)
           ((eq ch ?B) 200)
           ((eq ch ?C) 300)
           (t 0)))))

(defun planner-sort-tasks-by-date ()
  "Sort tasks by date, status and priority."
  (skip-chars-forward "#ABC")
  (let ((ch (char-before))
        status)
    (skip-chars-forward "0123456789 ")
    (setq status (char-after))
    (goto-char (planner-line-end-position))
    (skip-chars-backward "]) ")
    (format "%1c%1c%10s"
            (if (or (= status ?X)
                    (= status ?C))
                status ?\ )
            ch
            (if (= (skip-chars-backward "0123456789.")
                   -10)
                (buffer-substring (point)
                                  (+ 10 (point)))
              planner-sort-undated-tasks-equivalent))))

(defun planner-sort-tasks-by-link ()
  "Sort tasks by status, priority and link."
  (let ((info (planner-current-task-info)))
    (concat ;(read (current-buffer))
     (cond
      ((string= (planner-task-status info) "P") "1")
      ((string= (planner-task-status info) ">") "2")
      ((string= (planner-task-status info) "X") "3")
      ((string= (planner-task-status info) "C") "4")
      (t "0"))
     (planner-task-priority info)
     (or (planner-task-link info) ""))))

(defun planner-sort-tasks ()
  "Sort the tasks.
On day pages, sort according to priority and position.	On plan
pages, sort according to status, priority, date, and position."
  (interactive)
  (let ((case-fold-search nil)
	(planner-on-date-page (string-match planner-date-regexp
					    (planner-page-name)))
        (old-task (planner-current-task-info))
        (line-offset (- (point) (planner-line-beginning-position)))
        (old-point (point)))
    (goto-char (point-min))
    (while (re-search-forward "^#[A-C][0-9]*" nil t)
      (goto-char (match-beginning 0))
      (let ((here (point)))
        (while (and (char-after) (= (char-after) ?#))
          (forward-line 1))
        (save-restriction
          (narrow-to-region here (point))
          (goto-char here)
          (condition-case err
              (sort-subr nil
                         'forward-line 'end-of-line
                         planner-sort-tasks-key-function nil
                         nil)
            (wrong-number-of-arguments ; OLD EMACS, 5 args
             (sort-subr nil
                        'forward-line 'end-of-line
                        planner-sort-tasks-key-function nil)))
          (goto-char (point-max)))))
    (if old-task
        (progn
          (planner-find-task old-task)
          (forward-char line-offset))
      (goto-char old-point))
    nil))			     ; Must return nil because of write-file-functions

(defun planner-sort-tasks-maybe ()
  "Sort tasks depending on `planner-sort-tasks-automatically'."
  (when planner-sort-tasks-automatically
    (planner-sort-tasks)))

(defun planner-renumber-tasks ()
  "Update task numbering to be in sequence once again."
  (interactive)
  (let ((old-point (point)))
    (goto-char (point-min))
    (let ((counters (list (cons "A" 1) (cons "B" 1) (cons "C" 1))))
      (while (re-search-forward "^#\\([A-C]\\)\\([0-9]+\\)" nil t)
        (let ((counter (assoc (match-string 1) counters)))
          (replace-match (number-to-string (cdr counter)) t t nil 2)
          (setcdr counter (1+ (cdr counter))))))
    (goto-char old-point))
  nil)			     ; Must return nil because of write-file-functions

(defun planner-renumber-tasks-maybe ()
  "Renumber tasks depending on `planner-renumber-tasks-automatically'."
  (when planner-renumber-tasks-automatically
    (planner-renumber-tasks)))

(defun planner-fix-tasks ()
  "Sort, renumber and align tasks."
  (interactive)
  (planner-sort-tasks)
  (planner-renumber-tasks)
  (planner-align-tasks))

;;;_ + Notes

;;;###autoload
(defun planner-create-note (&optional page)
  "Create a note to be remembered in PAGE (today if PAGE is nil).
If `planner-reverse-chronological-notes' is non-nil, create the
note at the beginning of the notes section; otherwise, add it to
the end.  Position point after the anchor."
  (interactive (list (and (planner-derived-mode-p 'planner-mode)
                          (planner-page-name))))
  (planner-goto (or page
                    (and (planner-derived-mode-p 'planner-mode)
                         (planner-page-name))
                    (planner-today)))
  (planner-seek-to-first 'notes)
  (save-restriction
    (when (planner-narrow-to-section 'notes)
      (let ((total 0))
        (goto-char (point-min))
        (while (re-search-forward "^\\.#[0-9]+\\s-+" nil t)
          (setq total (1+ total)))
        (if planner-reverse-chronological-notes
            (progn (goto-char (point-min))
                   (forward-line 1)
                   (skip-chars-forward "\n"))
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (when (= (forward-line 1) 1) (insert "\n"))
          (when (= (forward-line 1) 1) (insert "\n")))
        (insert ".#" (number-to-string (1+ total)) " ")
        (unless (eobp) (save-excursion (insert "\n\n")))
        (1+ total)))))

(defun planner-delete-note ()
  "Delete the current note."
  (interactive)
  (save-window-excursion
    (let ((info (planner-current-note-info)))
      (when info
        (save-window-excursion
          (when (planner-jump-to-linked-note info)
            (save-restriction
              (planner-narrow-to-note)
              (delete-region (point-min) (point-max)))))
        (save-restriction
          (planner-narrow-to-note)
          (delete-region (point-min) (point-max)))))))

(defun planner-format-note (info &optional anchor title timestamp link body)
  "Return the string representation of INFO.
ANCHOR, TITLE, TIMESTAMP, LINK and BODY override INFO if present."
  (unless anchor (setq anchor (planner-note-anchor info)))
  (unless title (setq title (planner-note-title info)))
  (unless timestamp (setq timestamp (planner-note-timestamp info)))
  (unless link (setq link (planner-note-link info)))
  (unless body (setq body (planner-note-body info)))
  (concat (if (and anchor (not (string= "" anchor))) (concat ".#" anchor " ") "")
          title
          (if (and timestamp (not (string= "" timestamp))) (concat " " timestamp) "")
          (if (and link (not (string= "" link))) (concat " (" link ")") "")
          (if (and body (not (string= "" body))) body "")))

(defun planner-update-note ()
  "Copy the text from this note to the linked note, if any."
  (interactive)
  (save-window-excursion
    (let ((info (planner-current-note-info))
          text)
      (save-restriction
        (when (planner-narrow-to-note)
          (setq text (buffer-substring-no-properties (point-min) (point-max)))
          ;; Get rid of the anchor.
          (when (string-match "^\\.#[0-9]+\\s-+" text)
          (setq text (replace-match "" nil t text)))
          ;; Get rid of the link
          (when (string-match "\\s-+(\\[\\[.+?\\]\\])" text)
            (setq text (replace-match "" nil t text)))))
      ;; Insert the new body
      (when (planner-jump-to-linked-note)
        (save-restriction
          (when (planner-narrow-to-note)
            (goto-char (point-min))
            (skip-chars-forward ".#0-9")
            (delete-region (point) (point-max))
            (insert " " text)
            (goto-char (point-min))
            (goto-char (planner-line-end-position))
            (insert " ("
                    (planner-make-link
                     (concat (planner-note-page info) "#" (planner-note-anchor info)))
                    ")")))))))

;; Case 1a: Date and plan page exist, new plan page wanted
;; Case 1b: Date page exists, no plan yet, plan page wanted
;; Case 2: Date and plan page exist, no plan page wanted
;; Case 3: No date, just plan page
(defun planner-replan-note (page)
  "Change or assign the plan page for the current note.
PAGE-NAME is the new plan page for the note."
  (interactive
   (list (planner-read-non-date-page
          (planner-file-alist) nil
          (planner-note-link-text (planner-current-note-info)))))
  (let ((info (planner-current-note-info t)))
    (when (and page
               (or (string= page (planner-note-plan info))
                   (string= page (planner-note-date info))))
      (error "Same plan page"))
    (when (null (or page (planner-note-date info)))
      (error "Cannot unplan note without day page"))
    (save-window-excursion
      ;; Delete the old plan note
      (when (planner-note-plan info)
        (when (string-match planner-date-regexp (planner-note-page info))
          (planner-jump-to-linked-note info))
        (save-restriction
          (planner-narrow-to-note)
          (delete-region (point-min) (point-max))))
      (let (new)
        (when page
          ;; Create note on plan page
          (setq new (planner-create-note page))
          (insert (planner-format-note
                   info "" nil nil
                   (if (planner-note-date info)
                         (planner-make-link
                          (concat (planner-note-date info)
                                  "#"
                                  (planner-note-anchor info)))
                     ""))))
        ;; Update note on date page, if any
        (forward-line -1)
        (when (planner-note-date info)
            (if (string-match planner-date-regexp (planner-note-page info))
                (progn
                  (planner-find-file (planner-note-date info))
                  (goto-char (point-min))
                  (re-search-forward (concat "^\\.#" (planner-note-anchor info) "\\s-") nil t))
              (planner-jump-to-linked-note info))
            (save-restriction
              (planner-narrow-to-note)
              (delete-region (point-min) (point-max))
              (insert (planner-format-note
                       info nil nil nil
                       (if new
                           (planner-make-link
                            (concat (planner-link-base page) "#" (number-to-string new)))
                         "")))))))))

;; Improvements:
;;
;; - Link back to the task? If we can figure out how to stably link to
;;   a task in the first place...
;;
;; - Should plan-page-p default to t? be a customizable variable? What
;;   should it be called? I have the urge to write
;;   planner-create-note-from-task-behavior which can have the
;;   following values: 'day, 'plan, 'copy, 'xref ...

(defun planner-create-note-from-task (&optional plan-page-p)
  "Create a note based on the current task.
Argument PLAN-PAGE-P is used to determine whether we put the new
note on the task's plan page or on the current page."
  (interactive "P")
  (let ((task-info (planner-current-task-info))
         note-num)
    (when task-info
      ;; If PLAN-PAGE-P and the task has a plan page, create a note on
      ;; the plan page. If not, create it on the current page.
      (when (and plan-page-p
                 (string= (planner-task-date task-info)
                          (planner-task-page task-info)))
        (planner-jump-to-linked-task task-info))
      (setq note-num (planner-create-note (planner-page-name)))
      (save-excursion
        (save-window-excursion
          (when (planner-find-task task-info)
            (planner-edit-task-description
             (concat (planner-task-description task-info) " "
                     (planner-make-link
                      (concat (planner-page-name) "#"
                              (number-to-string note-num))
                      (format "(%d)" note-num)))))))
      (insert " " (planner-task-description task-info) "\n\n"))))

(defun planner-narrow-to-note (&optional page note-number)
  "Narrow to the specified note.  Widen and return nil if note is not found.
If PAGE is nil, use current page.
If NOTE-NUMBER is nil, use current note.
Undefined behavior if PAGE is (non-nil and not today) and NOTE-NUMBER is nil."
  (when page (planner-goto page))
  (save-excursion
    (let (beginning)
      (if note-number
          (progn
            (goto-char (point-min))
            (when (re-search-forward (concat "^\\.#" note-number) nil t)
              (setq beginning (match-beginning 0))))
        (goto-char (planner-line-end-position))
        (when (re-search-backward "^\\.#[0-9]+" nil t)
          (setq beginning (planner-line-beginning-position))))
      (when beginning
        ;; Search for the end
        (forward-line 1)
        (narrow-to-region
         beginning
         (min
          (or (save-excursion (and (re-search-forward "^\\.#" nil t) (match-beginning 0))) (point-max))
          (or (save-excursion (and (re-search-forward "^* " nil t) (match-beginning 0))) (point-max))))
        t))))

(defun planner-note-page (note-info)
  "Return the page specified by NOTE-INFO."
  (elt note-info 0))
(defun planner-note-anchor (note-info)
  "Return the anchor specified by NOTE-INFO."
  (elt note-info 1))
(defun planner-note-title (note-info)
  "Return the title specified by NOTE-INFO."
  (elt note-info 2))
(defun planner-note-timestamp (note-info)
  "Return the timestamp specified by NOTE-INFO."
  (elt note-info 3))
(defun planner-note-link (note-info)
  "Return the link specified by NOTE-INFO."
  (elt note-info 4))
(defun planner-note-link-text (note-info)
  "Return the link text specified by NOTE-INFO."
  (elt note-info 4))
(defun planner-note-body (note-info)
  "Return the timestamp specified by NOTE-INFO."
  (elt note-info 5))

(defun planner-note-date (note-info)
  "Return the date for NOTE-INFO."
  (cond
   ((string-match planner-date-regexp (planner-note-page note-info))
    (planner-note-page note-info))
   ((and (planner-note-link note-info)
         (string-match planner-date-regexp (planner-note-link note-info)))
    (planner-link-base (planner-note-link note-info)))))

(defun planner-note-plan (note-info)
  "Return the date for NOTE-INFO."
  (cond
   ((null (string-match planner-date-regexp (planner-note-page note-info)))
    (planner-note-page note-info))
   ((and (planner-note-link note-info)
         (null (string-match planner-date-regexp (planner-note-link note-info))))
    (planner-link-base (planner-note-link note-info)))))

(defun planner-current-note-info (&optional include-body)
  "Parse the current note and return the note information as a list.
The list is of the form (PAGE ANCHOR TITLE TIMESTAMP LINK BODY).
If INCLUDE-BODY is non-nil, the list will include the body of the
note."
  (save-excursion
    (save-restriction
      (when (planner-narrow-to-note)
        (goto-char (point-min))
        (when (looking-at "^\\.#\\([0-9]+\\)\\s-+\\(.+\\)")
          (let ((anchor (planner-match-string-no-properties 1))
                (title (planner-match-string-no-properties 2))
                timestamp link)
            (when (string-match
                   (concat
                    "\\s-+(\\("
                   (if (featurep 'planner-multi)
                       (concat "\\(" emacs-wiki-name-regexp "\\)"
                               "\\("
                               (regexp-quote planner-multi-separator)
                               emacs-wiki-name-regexp
                               "\\)*")
                     emacs-wiki-name-regexp)
                   "\\))\\s-*$")
                   title)
              (setq link (planner-match-string-no-properties 1 title))
              (setq title (replace-match "" nil t title)))
            (when (string-match "\\s-*\\([0-9]+:[0-9][0-9]\\)" title)
              (setq timestamp (planner-match-string-no-properties 1 title))
              (setq title (replace-match "" nil t title)))
            (list (planner-page-name) anchor title timestamp link
                  (and include-body (buffer-substring-no-properties
                                     (planner-line-end-position)
                                     (point-max))))))))))

(defun planner-search-notes-internal (regexp &optional limit include-body)
  "Return an alist of all notes in daily plan pages containing REGEXP.
The alist is of the form ((REFERENCE TITLE BODY) (REFERENCE TITLE BODY)
...). If LIMIT is non-nil, do not search days before LIMIT. If
INCLUDE-BODY is non-nil, return the body text, else return nil."
  (let ((pages (planner-get-day-pages limit t))
        (list (planner-file-alist))
        filename
        page start anchor text results title page-results)
    (while pages
      (setq page (car pages)
            filename (cdr (assoc page list)))
      (with-temp-buffer
        (when (file-readable-p filename)
          (insert-file-contents filename)
          (setq start nil)
          (setq page-results nil)
          ;; Find the first note
          (when (re-search-forward "\\.\\(#[0-9]+\\)\\s-+\\(.*\\)" nil t)
            (setq start (match-beginning 2))
            (setq anchor (match-string 1))
            (setq title (match-string 2)))
          (while (re-search-forward "\\.\\(#[0-9]+\\)\\s-+\\(.*\\)" nil t)
            ;; The text between start and (1- (match-beginning 0))
            ;; is the note body.
            (when (save-excursion
                    (save-match-data (re-search-backward regexp start t)))
              (add-to-list 'page-results
                           (list (concat page anchor)
                                 title
                                 (if include-body
                                     (buffer-substring-no-properties
                                      start
                                      (point))))))
            (setq start (match-beginning 2))
            (setq anchor (match-string 1))
            (setq title (match-string 2)))
          (when start
            (goto-char (point-max))
            (when (save-excursion (re-search-backward regexp start t))
              (add-to-list 'page-results
                           (list (concat page anchor)
                                 title
                                 (if include-body
                                     (buffer-substring-no-properties
                                      start
                                      (point)))))))
          (when planner-reverse-chronological-notes
            (setq page-results (nreverse page-results))))
        (setq results (append page-results results)))
      (setq pages (cdr pages)))
  results))

(defun planner-jump-to-linked-note (&optional note-info)
  "Display the note linked to by the current note or NOTE-INFO if non-nil."
  (interactive)
  (setq note-info (or note-info (planner-current-note-info)))
  (when (planner-note-link note-info)
    (planner-visit-link (planner-note-link note-info))
    (widen)
    t))

(defun planner-renumber-notes ()
  "Update note numbering."
  (interactive)
  (let ((old-point (point))
        (counter 1))
    (goto-char
     (if planner-reverse-chronological-notes (point-max) (point-min)))
    (while (if planner-reverse-chronological-notes
               (re-search-backward "^\\.#\\([0-9]+\\)" nil t)
             (re-search-forward "^\\.#\\([0-9]+\\)" nil t))
      (replace-match (number-to-string counter) t t nil 1)
      (when planner-reverse-chronological-notes
        (goto-char (planner-line-beginning-position)))
      (setq counter (1+ counter)))
    (goto-char old-point))
  nil)   ; Must return nil because of write-file-functions

(defun planner-renumber-notes-maybe ()
  "Renumber notes depending on `planner-renumber-notes-automatically'."
  (when planner-renumber-notes-automatically
    (planner-renumber-notes)))

;;;_ + Highlighting

(defgroup planner-fontlock nil
  "Font-locking for planner.el pages."
  :prefix "planner-"
  :group 'planner)

(defface planner-completed-task-face
  (if (featurep 'xemacs)
      '((t (:strikethru t :foreground "gray")))
    '((t (:strike-through t :foreground "gray"))))
  "Face for completed tasks."
  :group 'planner-fontlock)

(defface planner-cancelled-task-face
  (if (featurep 'xemacs)
      '((t (:strikethru t :foreground "gray")))
    '((t (:strike-through t :foreground "gray"))))
  "Face for cancelled tasks."
  :group 'planner-fontlock)

(defface planner-delegated-task-face
  '((t (:underline t)))
  "Face for delegated tasks."
  :group 'planner-fontlock)

(defface planner-in-progress-task-face
  (if (featurep 'xemacs)
      '((t (:bold t)))
    '((t (:slant oblique))))
  "Face for tasks in progress."
  :group 'planner-fontlock)
(defface planner-high-priority-task-face '((t (:foreground "red")))
  "Face for high-priority tasks."
  :group 'planner-fontlock)
(defface planner-medium-priority-task-face '((t (:foreground "green")))
  "Face for medium-priority tasks."
  :group 'planner-fontlock)
(defface planner-low-priority-task-face '((t (:foreground "blue")))
  "Face for low-priority tasks."
  :group 'planner-fontlock)

(defface planner-note-headline-face
  '((((class color) (background light))
     (:foreground "dark slate blue" :bold t))
    (((class color) (background dark))
     (:foreground "pale turquoise" :bold t))
    (t (:bold t)))
  "Face for note headlines."
  :group 'planner-fontlock)

;; Thanks to Oliver (oik AT gmx DOT net)
(defun planner-align-tasks ()
  "Align tasks neatly.
You can add this to `write-file-functions'to have the tasks
automatically lined up whenever you save.  For best results,
ensure `planner-align-tasks' is run after
`planner-renumber-tasks'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\([A-C]\\)\\([0-9]*\\)\\(\\s-+\\)" nil t)
      (replace-match
       ;; Ugly hack!
       ;; (make-string (max (- (length (match-string 2))) 0) ?\s)
       ;; is better, but relies on a CVSism.
       (let ((length (length (match-string 2))))
         (cond
          ((= length 0) "   ")
          ((= length 1) "  ")
          (t " ")))
       t t nil 3)))
  nil)  ; Return nil so that we can add this to write-file-functions

(defun planner-align-tasks-maybe ()
  "Align tasks depending on `planner-align-tasks-automatically'."
  (when planner-align-tasks-automatically
    (planner-align-tasks)))

;; FIXME: Is there a better way to do this?

(defun planner-highlight-region (beg end identifier priority properties)
  "Add the specified text properties to the overlay or region.
BEG and END are the start and end of the region.  IDENTIFIER is a
symbol that identifies this particular overlay.  PRIORITY controls
how important this overlay is.  PROPERTIES is a list of properties
or attributes to apply."
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-properties extent properties)
        (set-extent-property extent 'priority priority))
    (if (functionp 'overlay-put)
        (progn
          (let ((overlay (make-overlay beg end)))
            (overlay-put overlay identifier t)
            (overlay-put overlay 'planner t)
            (overlay-put overlay 'priority priority)
            (while properties
              (overlay-put overlay (car properties) (cadr properties))
              (setq properties (cddr properties)))))
      (add-text-properties beg end properties))))

(defcustom planner-hide-task-status-when-highlighting nil
  "*If non-nil, hide task status when font-locking."
  :type 'boolean
  :group 'planner-fontlock)

(defun planner-highlight-tasks (beg end &optional verbose)
  "Highlight tasks from BEG to END.  VERBOSE is ignored."
  (goto-char beg)
  (while (re-search-forward (concat "^#\\([A-C]\\)\\([0-9]*\\)\\s-+\\("
                                    planner-marks-regexp
                                    "\\)\\s-") end t)
    (let ((mark (match-string 3))
          (priority (match-string 1))
          faces)
      (setq faces
            (append
             (cond
              ((string= priority "A") '(planner-high-priority-task-face))
              ((string= priority "B") '(planner-medium-priority-task-face))
              ((string= priority "C") '(planner-low-priority-task-face)))
             (cond
              ((string= mark "X") '(planner-completed-task-face))
              ((string= mark ">") '(planner-delegated-task-face))
              ((string= mark "C") '(planner-cancelled-task-face))
              ((string= mark "o") '(planner-in-progress-task-face)))))
      (if (featurep 'xemacs)
	  (mapcar (lambda (face)
		    (when face
		      (planner-highlight-region
		       (match-beginning 0) (match-end 3) 'planner-task 50
		       (list 'face face)))) faces)
	(planner-highlight-region
	 (match-beginning 0) (match-end 3) 'planner-task 50
         (list 'face (mapcar 'face-attr-construct faces))))
      (planner-highlight-region
       (match-end 3) (planner-line-end-position)
       'planner-task
       51
       (list 'face
             (cond
              ((string= mark "X") 'planner-completed-task-face)
              ((string= mark ">") 'planner-delegated-task-face)
              ((string= mark "C") 'planner-cancelled-task-face)
              ((string= mark "o") 'planner-in-progress-task-face))))
      (when planner-hide-task-status-when-highlighting
        (planner-highlight-region
         (match-beginning 3) (1+ (match-end 3))
         'planner-task
         50
         (list 'invisible t))))))

(defun planner-highlight-notes (beg end &optional verbose)
  "Highlight notes as second-level headers from BEG to END.
VERBOSE is ignored."
  (goto-char beg)
  (while (re-search-forward "^\\.#\\([0-9]+\\) [^(\n]+" end t)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     '(face planner-note-headline-face))))

(defun planner-notes-get-headlines (&optional limit)
  "Return note headlines on the current page.
If LIMIT is non-nil, return only that many from the top."
  (when (stringp limit) (setq limit (string-to-number limit)))
  (let (headlines)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and
                (re-search-forward "^.\\(#[0-9]+\\)\\s-+\\(.+\\)" nil t)
                (if limit
                    (> limit 0)
                  t))
          (add-to-list
           'headlines
           (cons
            (planner-match-string-no-properties 1)
            (planner-match-string-no-properties 2))
           t)
          (if limit (setq limit (1- limit))))))
    headlines))

(defun planner-notes-tag (beg end)
  "Replace the region BEG to END with the notes for this page."
  (delete-region beg end)
  (let ((name (planner-page-name))
        (case-fold-search nil))
    (mapcar
     (lambda (item)
       (when (string-match
              (if (featurep 'planner-multi)
                  (concat
                   "\\s-*(\\(\\(" emacs-wiki-name-regexp "\\)"
                   planner-multi-separator "\\)*"
                   "\\(" emacs-wiki-name-regexp "\\))\\s-*$")
                (concat "\\s-*(\\(" emacs-wiki-name-regexp "\\))\\s-*$"))
              (cdr item))
         (setcdr item (replace-match "" nil t (cdr item))))
       (insert " - " (planner-make-link (concat name
                                               (car item))
                                       (cdr item)) "\n"))
     (planner-notes-get-headlines))))

(defun planner-past-notes-tag (beg end &optional attrs)
  "Replace the region BEG to END with an index of past notes.
If ATTRS is non-nil, it is an alist containing values for
DIRECTORY and START."
  (let ((files (planner-get-day-pages nil nil t))
        (earliest (cdr (assoc "start" attrs)))
        (list (planner-file-alist)))
    (while files
      (when (or (null earliest)
                (not (string-lessp (car files) earliest)))
        (let ((title-lines (list t)))
          (with-temp-buffer
            (insert-file-contents (cdr (assoc (car files) list)))
            (while (re-search-forward "^\\.#\\([0-9]+\\)\\s-+\\(.+\\)" nil t)
              (nconc title-lines (list (cons (match-string 1)
                                             (match-string 2))))))
          (setq title-lines (cdr title-lines))
          (when title-lines
            (insert (planner-make-link (planner-page-name (car files))) " ::\n")
            (insert "  <dl class=\"contents\">\n")
            (while title-lines
              (insert
               (format "  <dt class=\"contents\">[[%s#%s][%s]]</dt>\n"
                       (planner-page-name (car files))
                       (caar title-lines) (cdar title-lines)))
              (setq title-lines (cdr title-lines)))
            (insert "  </dl>\n\n"))))
      (setq files (cdr files)))))

;;;_ + Markup

(defun planner-markup-note ()
  "Replace note with marked-up span."
  (replace-match
   (format "#%s\n\n** " (match-string 1))))

(defun planner-markup-task ()
  "Replace tasks with marked-up spans."
  (save-match-data
    (let ((priority (match-string 1))
          (number (match-string 2))
          (status (match-string 3))
          (text (match-string 4)))
      (insert
       " - <div class=\""
       (cond
        ((string= status "_") "task_")
        ((string= status "o") "tasko")
        ((string= status ">") "taskd")
        ((string= status "P") "taskp")
        ((string= status "X") "taskX")
        ((string= status "C") "task_cancelled")
        (t "task"))
       "\">")
      (insert
       "<span class=\""
       (cond
        ((string= priority "A") "taskA")
        ((string= priority "B") "taskB")
        ((string= priority "C") "taskC")
        (t "task"))
       "\">"
       priority number " " status " </span> ")
      (insert text "</div>")))
  "")

;;;_* Indexing

;; I want to compress the list of day pages. Arranging them by month
;; may be a good strategy, although a calendar would be optimal.

(defun planner-index ()
  "Display an index of all known Wiki pages."
  (interactive)
  (let ((emacs-wiki-project planner-project))
    (message "Generating Wiki index...")
    (pop-to-buffer (planner-generate-index))
    (goto-char (point-min))
    (planner-mode)
    (message "Generating Wiki index...done")))

(defun planner-generate-index (&optional as-list exclude-private)
  "Generate an index of all Wiki pages.
List planner pages separately. If AS-LIST is non-nil, format it
as a list. If EXCLUDE-PRIVATE is non-nil, exclude anything for
which `emacs-wiki-private-p' returns non-nil. You may also be
interested in the `emacs-wiki-index-title-threshold' variable
for controlling whether #title is used in the index."
  (with-current-buffer (get-buffer-create "*Wiki Index*")
    (erase-buffer)
    (cd planner-directory)
    (emacs-wiki-change-project planner-project)
    (let ((files (emacs-wiki-index-files-list t))
          dates month last-month)
      (while files
        (unless (and exclude-private
                     (emacs-wiki-private-p
                      (emacs-wiki-index-file-page (car files))))
          (if (string-match
               planner-date-regexp
               (emacs-wiki-index-file-page (car files)))
              (setq dates
                    (append
                     (list (emacs-wiki-index-file-page (car files)))
                     dates))
            (insert
             (if as-list "- " "")
             (emacs-wiki-make-link
              (emacs-wiki-index-file-page (car files))
              (emacs-wiki-index-file-title (car files)))
             "\n")))
        (setq files (cdr files)))
      (when dates
        (if planner-publish-dates-first-p
            (goto-char (point-min))
          (insert "\n"))
;;        (setq dates (nreverse dates))
        (while dates
          (setq month (substring (car dates) 0 7))
          (unless (string= month last-month)
            (setq last-month month)
            (insert "\n" month " |"))
          (insert " [[" (car dates) "][."
                  (substring (car dates) 8)
                  " ]]")
          (setq dates (cdr dates)))
        (when planner-publish-dates-first-p
          (insert "\n"))))
    (current-buffer)))

(defadvice emacs-wiki-generate-index (around planner-experimental activate)
  "Create a separate index for planner day pages."
  (setq ad-return-value
        (if (planner-derived-mode-p 'planner-mode)
            (planner-generate-index (ad-get-arg 0)
                                    (ad-get-arg 1))
          ad-do-it)))

;;;_  + Info

(defun planner-annotation-from-info ()
  "If called from an info node, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (eq major-mode 'Info-mode)
    (planner-make-link
     (concat "info://" Info-current-file "#" Info-current-node)
     (if (and (not (equal Info-current-file "dir"))
              (equal Info-current-node "Top"))
         (file-name-nondirectory Info-current-file)
       Info-current-node)
     t)))

(add-hook 'planner-annotation-functions 'planner-annotation-from-info)
(custom-add-option 'planner-annotation-functions 'planner-annotation-from-info)

;;;_ + Common mail functions

(defun planner-get-name-from-address (address)
  "Return the name for ADDRESS to be used in links."
  (let ((addr (mail-extract-address-components address)))
    (or (car addr) (cadr addr))))

;;;_* User functions

;;;_ + Navigation

(defun planner-page-file (page &optional no-check-p)
  "Return a filename if PAGE exists within `planner-project'.
If NO-CHECK-P is non-nil, the planner project files are always updated."
  (with-temp-buffer
    (let ((emacs-wiki-project planner-project)
          (emacs-wiki-current-project planner-project))
      (unless (emacs-wiki-file-alist)
        (emacs-wiki-refresh-file-alist))
      (emacs-wiki-page-file page no-check-p))))

;;;###autoload
(defun plan (&optional force-days)
  "Start your planning for the day, carrying unfinished tasks forward.

If FORCE-DAYS is a positive integer, search that many days in the past
for unfinished tasks.
If FORCE-DAYS is 0 or t, scan all days.
If FORCE-DAYS is nil, use the value of `planner-carry-tasks-forward'
instead, except t means scan only yesterday."
  ;; Special treatment of t for planner-carry-tasks-forward is for
  ;; backward compatibility.
  (interactive "P")
  ;; Special check if the directory exists
  (unless (file-directory-p planner-directory)
    (make-directory planner-directory t))
  (planner-update-wiki-project)
  (if planner-use-day-pages
      (progn
        (unless force-days
          (setq force-days
                (if (equal planner-carry-tasks-forward t)
                    1
                  planner-carry-tasks-forward)))
        (when (and (integerp force-days)
                   (= force-days 0))
          (setq force-days t))
        (planner-goto-today)
        (let* ((today (planner-today))
               (names (planner-get-day-pages nil (planner-yesterday)))
               (today-buffer (current-buffer))
               (planner-tasks-file-behavior
                ;; Force saving so that the file list can be updated
                (or planner-tasks-file-behavior
                    'save))
               (planner-use-other-window nil)
               (live-buffers (and
                              (equal planner-tasks-file-behavior
                                     'close)
                              (buffer-list))))
          ;; Limit the list for force-days
            (when (and (integerp force-days)
                       (> (length names) force-days))
              (setcdr (nthcdr (1- force-days) names) nil))
            (when force-days
              (while names
                (planner-find-file (car names))
                ;; Attempt to copy all the tasks
                (when (not (equal today (planner-page-name)))
                  (let ((planner-tasks-file-behavior nil))
                    (planner-copy-or-move-region 1 (1+ (buffer-size))
                                                 (planner-today) t))
                  (unless (buffer-modified-p)
                    (kill-buffer (current-buffer))))
                (setq names (cdr names))))
            ;; Jump to the most recent daily page
            (if (or planner-carry-tasks-forward
                    (planner-page-file today)
                    (null names))
                (planner-goto-today)
              (planner-goto (car names)))
            ;; Save/kill files if configured to do so
            (when planner-tasks-file-behavior
              (planner-save-buffers live-buffers))
            (with-planner
              (emacs-wiki-refresh-file-alist))))
    (planner-find-file "WelcomePage")))

(defvar planner-goto-hook '(planner-seek-to-first)
  "Functions called after a planner page is opened.")

;;;###autoload
(defun planner-goto (date &optional just-show)
  "Jump to the planning page for DATE.
If no page for DATE exists and JUST-SHOW is non-nil, don't create
a new page - simply return nil."
  (interactive (list (or
                      (planner-read-date)
                      (planner-read-non-date-page (planner-file-alist)))))
  (if (or (not just-show) (planner-page-exists-p date))
      (progn
        (planner-find-file date
                           (if planner-use-other-window
                               'find-file-other-window
                             'find-file))
        (widen)
        (goto-char (point-min))
        (run-hooks 'planner-goto-hook)
        ;; planner-goto-hook returns nil
        t)
    ;; File not found, and not supposed to be created.
    (when (interactive-p)
      (message "No planner file for %s." date))
    ;; return nil
    nil))

;;;###autoload
(defun planner-goto-plan-page (page)
  "Opens PAGE in the the `planner-project' wiki.
Use `planner-goto' if you want fancy calendar completion."
  (interactive (list (planner-read-name (planner-file-alist))))
  (planner-find-file page))

;;;###autoload
(defun planner-show (date)
  "Show the plan page for DATE in another window, but don't select it.
If no page for DATE exists, return nil."
  (interactive (list (planner-read-date)))
  (save-selected-window
    (let ((planner-use-other-window t))
      (planner-goto date planner-show-only-existing))))

;;;###autoload
(defun planner-goto-today ()
  "Jump to the planning page for today."
  (interactive)
  (planner-goto (planner-today)))

;;;###autoload
(defun planner-goto-most-recent ()
  "Go to the most recent day with planning info."
  (interactive)
  (planner-goto
   (planner-get-previous-existing-day
    (planner-calculate-date-from-day-offset
     (planner-get-current-date-filename) 1))))

;;;###autoload
(defun planner-goto-yesterday (&optional days)
  "Goto the planner page DAYS before the currently displayed date.
If DAYS is nil, goes to the day immediately before the currently
displayed date.  If the current buffer is not a daily planner
page, calculates date based on today."
  (interactive "p")
  (let ((planner-use-other-window nil))
    (planner-goto (planner-calculate-date-from-day-offset
                   (planner-get-current-date-filename) (or (- days) -1)))))

;;;###autoload
(defun planner-goto-tomorrow (&optional days)
  "Goto the planner page DAYS after the currently displayed date.
If DAYS is nil, goes to the day immediately after the currently
displayed date.  If the current buffer is not a daily planner
page, calculates date based on today."
  (interactive "p")
  (let ((planner-use-other-window nil))
    (planner-goto (planner-calculate-date-from-day-offset
                   (planner-get-current-date-filename) (or days 1)))))

;;;###autoload
(defun planner-goto-previous-daily-page ()
  "Goto the last plan page before the current date.
The current date is taken from the day page in the current
buffer, or today if the current buffer is not a planner page.
Does not create pages if they do not yet exist."
  (interactive)
  (let ((planner-use-other-window nil))
    (planner-goto (planner-get-previous-existing-day
                   (planner-get-current-date-filename)))))

;;;###autoload
(defun planner-goto-next-daily-page ()
  "Goto the first plan page after the current date.
The current date is taken from the day page in the current
buffer, or today if the current buffer is not a planner page.
Does not create pages if they do not yet exist."
  (interactive)
  (let ((planner-use-other-window nil))
    (planner-goto (planner-get-next-existing-day
                   (planner-get-current-date-filename)))))

;;;_ + Tasks

;;;_  + Creating

;;;###autoload
(defun planner-create-high-priority-task-from-buffer ()
  "Create a high-priority task based on this buffer.
Do not use this in LISP programs. Instead, set the value of
`planner-default-task-priority' and call `planner-create-task' or
`planner-create-task-from-buffer'."
  (interactive)
  (let ((planner-default-task-priority "A"))
    (call-interactively 'planner-create-task-from-buffer)))

;;;###autoload
(defun planner-create-medium-priority-task-from-buffer ()
  "Create a high-priority task based on this buffer.
Do not use this in LISP programs. Instead, set the value of
`planner-default-task-priority' and call `planner-create-task' or
`planner-create-task-from-buffer'."
  (interactive)
  (let ((planner-default-task-priority "B"))
    (call-interactively 'planner-create-task-from-buffer)))

;;;###autoload
(defun planner-create-low-priority-task-from-buffer ()
  "Create a high-priority task based on this buffer.
Do not use this in LISP programs. Instead, set the value of
`planner-default-task-priority' and call `planner-create-task' or
`planner-create-task-from-buffer'."
  (interactive)
  (let ((planner-default-task-priority "C"))
    (call-interactively 'planner-create-task-from-buffer)))

(defun planner-read-task ()
  "Return a list of information for a task."
  (list
   (read-string "Describe task: ")
   (when planner-use-day-pages
     (cond
      ;; Universal prefix means pick up from current page
      ((and current-prefix-arg
            (planner-derived-mode-p 'planner-mode)
            (string-match planner-date-regexp (planner-page-name)))
       (planner-page-name))
      ;; Date selected in calendar
      ((condition-case nil (calendar-cursor-to-date) (error nil))
       (planner-date-to-filename (calendar-cursor-to-date)))
      ;; Prompt for date
      (t (let ((planner-expand-name-favor-future-p
                (or planner-expand-name-favor-future-p
                    planner-task-dates-favor-future-p)))
           (planner-read-date)))))
   (when planner-use-plan-pages
     (if (and current-prefix-arg (planner-derived-mode-p 'planner-mode)
              (not (string-match planner-date-regexp (planner-page-name))))
         ;; Universal prefix means pick up from current page
         (planner-page-name)
       (let ((planner-default-page
              (if (and (planner-derived-mode-p 'planner-mode)
                       (planner-page-name)
                       (not (string-match planner-date-regexp
                                          (planner-page-name))))
                  (planner-page-name)
                planner-default-page)))
         (planner-read-non-date-page
          (planner-file-alist)))))
   planner-default-task-status))

;; NOTE: Prefix arg changed to prompt for PLAN-PAGE instead of
;; set to today
;;;###autoload
(defun planner-create-task-from-buffer (title date &optional plan-page status)
  "Create a new task named TITLE on DATE based on the current buffer.
With a prefix, do not prompt for PLAN-PAGE. The task is
associated with PLAN-PAGE if non-nil. If STATUS is non-nil, use
that as the status for the task. Otherwise, use
`planner-default-task-status'. See `planner-create-task' for more
information."
  (interactive (planner-read-task))
  (setq planner-default-page plan-page)
  (let ((planner-create-task-hook (append planner-create-task-from-buffer-hook
                                          planner-create-task-hook))
	(annotation (run-hook-with-args-until-success
		     'planner-annotation-functions)))
    (when (and planner-annotation-symbol-string
               (string-match planner-annotation-symbol-string title))
      (setq title (replace-match (or annotation "") t t title)
            annotation nil))
    (planner-create-task title
                         (when (and date
                                    (string-match planner-date-regexp date))
                           date)
                         annotation
                         plan-page
                         status)))

(defun planner-create-task (title date &optional annotation plan-page status)
  "Create a new task named TITLE based on the current Wiki page.
If DATE is non-nil, makes a daily entry on DATE, else makes an
entry in today's planner page. It's assumed that the current Wiki
page is the page you're using to plan an activity. Any time
accrued to this task will be applied to that page's name in the
timelog file, assuming you use timeclock. If ANNOTATION is
non-nil, it will be used for the page annotation. If PLAN-PAGE is
non-nil, the task is associated with the given page. If STATUS is
non-nil, use that as the status for the task. Otherwise, use
`planner-default-task-status'.

If called with an interactive prefix argument, do not prompt for
PLAN-PAGE.

You probably want to call `planner-create-task-from-buffer' instead."
  (interactive
   (list
    (read-string "Describe task: ")
    (when planner-use-day-pages
      (cond
       ;; Universal prefix means pick up from current page
       ((and current-prefix-arg
             (planner-derived-mode-p 'planner-mode)
             (string-match planner-date-regexp (planner-page-name)))
        (planner-page-name))
       ;; Date selected in calendar
       ((condition-case nil (calendar-cursor-to-date) (error nil))
        (planner-date-to-filename (calendar-cursor-to-date)))
       ;; Prompt for date
       (t (let ((planner-expand-name-favor-future-p
                 (or planner-expand-name-favor-future-p
                     planner-task-dates-favor-future-p)))
            (planner-read-date)))))
    nil ;; No annotation, interactively
    (when planner-use-plan-pages
      (if (and current-prefix-arg (planner-derived-mode-p 'planner-mode)
               (not (string-match planner-date-regexp (planner-page-name))))
          ;; Universal prefix means pick up from current page
          (planner-page-name)
        (let ((planner-default-page
               (if (and (planner-derived-mode-p 'planner-mode)
                        (planner-page-name)
                        (not (string-match planner-date-regexp
                                           (planner-page-name))))
                   (planner-page-name)
                 planner-default-page)))
          (planner-read-non-date-page
           (planner-file-alist)))))
    planner-default-task-status))
  (setq planner-default-page plan-page)
  (planner-create-task-from-info
   nil ; info
   planner-default-task-priority    ; priority
   "0"    ; number
   (or status planner-default-task-status)    ; status
   (if (and annotation
            (not (string= annotation ""))
            (or (not plan-page)
                (and
                 (not (string= plan-page annotation))
                 (not (string= (concat "[[" plan-page "]]") annotation)))))
       ;; Used C-u to make a plan-page annotation, so preserve
       ;; the context
       (concat title " : " annotation)
     title)  ; description
   ;; link: If creating this from a planner plan page, use the
   ;; current page name
   (and plan-page (planner-make-link plan-page)) ; link text
   date
   plan-page))

(defun planner-create-task-from-note (title date &optional plan-page status)
  "Create a task based on the current note with TITLE, DATE, PLAN-PAGE and STATUS.
A more do-what-I-mean way to do this is to position point on the first
line of a note (.#1 ...) and call `planner-create-task-from-buffer'."
  (interactive (let* ((info (planner-current-note-info))
                      (planner-default-page (and info (planner-note-plan info))))
                 (if info
                     (planner-read-task)
                   (error "Not in a planner note"))))
  (let* ((info (planner-current-note-info))
         (annotation (planner-make-link (concat (planner-note-page info)
                                                "#"
                                                (planner-note-anchor info)))))
    (when (and planner-annotation-symbol-string
               (string-match planner-annotation-symbol-string title))
      (setq title (replace-match (or annotation "") t t title)
            annotation nil))
    (planner-create-task title
                         (when (and date (string-match planner-date-regexp date))
                           date)
                         annotation
                         plan-page
                         status)))


;;;_  + Rescheduling

(defvar planner-copy-or-move-task-suppress-duplicates t
  "*If non-nil, do not create duplicate tasks.")

(defvar planner-replan-task-function 'planner-replan-task-basic
  "Function called when replanning a task. Should accept PAGE-NAME.")

(defun planner-replan-task (page-name)
  "Change or assign the plan page for the current task.
PAGE-NAME is the new plan page for the task. Use
`planner-copy-or-move-task' if you want to change the date.
With a prefix, provide the current link text for editing."
  (interactive (list
                (planner-read-name
                 (planner-file-alist) nil
                 (when current-prefix-arg
                   (planner-task-link-text (planner-current-task-info))))))
  (funcall planner-replan-task-function page-name))

(defun planner-replan-task-basic (page-name)
  "Change or assign the plan page for the current task.
PAGE-NAME is the new plan page for the task. Use
`planner-copy-or-move-task' if you want to change the date.
With a prefix, provide the current link text for editing."
  (let ((info (planner-current-task-info)))
    (when (and info (not (equal page-name (planner-task-plan info))))
      (with-planner-update-setup
       ;; Delete from old plan page
       (when (planner-task-plan info)
         (planner-find-file (planner-task-plan info))
         (when (planner-find-task info)
           (delete-region (planner-line-beginning-position)
                          (1+ (planner-line-end-position)))))
       ;; Add to new plan page, if any, and update date
       (if page-name
           (progn
             (planner-find-file page-name)
             (planner-seek-task-creation-point)
             (insert (planner-format-task info nil nil nil nil
                                          (or (planner-task-date info)
                                              "")
                                          (or (planner-task-date info)
                                              ""))
                     "\n")
             (forward-line -1)
             (planner-update-task))
         ;; Else, go to day page and update line
         (planner-find-file (planner-task-date info))
         (if (planner-find-task info)
             (delete-region (planner-line-beginning-position)
                            (1+ (planner-line-end-position)))
           (planner-seek-task-creation-point))
         (insert (planner-format-task info nil nil nil nil
                                      (or (planner-make-link page-name) "")
                                      (or page-name ""))
                 "\n"))))))

(defun planner-seek-task-creation-point ()
  "Jump to point where task would be created."
  (planner-seek-to-first (cdr (assoc 'tasks planner-sections)))
  (when planner-add-task-at-end-flag
    (while (looking-at "^#")
      (forward-line))
    (unless (bolp) (insert "\n"))))

(defun planner-copy-or-move-task-basic (&optional date force)
  "Move the current task to DATE.
If this is the original task, it copies it instead of moving.
Most of the time, the original should be kept in a planning file,
but this is not required. If FORCE is non-nil, the task is moved
regardless of status. It also works for creating tasks from a
Note. Use `planner-replan-task' if you want to change the plan
page in order to get better completion.
This function is the most complex aspect of planner.el."
  (interactive (list (let ((planner-expand-name-favor-future-p
                            (or planner-expand-name-favor-future-p
                                planner-task-dates-favor-future-p)))
                       (planner-read-date))
                     current-prefix-arg))
  (if (or (null date)
          (string-match planner-date-regexp date))
      (let ((live-buffers (when (equal planner-tasks-file-behavior 'close)
                            (buffer-list))))
        (when (equal date (planner-page-name))
          (error "Cannot move a task back to the same day"))
        (save-excursion
          (save-window-excursion
            (save-restriction
              (beginning-of-line)
              (let* ((task-info (planner-current-task-info))
                     (plan-page (planner-task-plan task-info))
                     (date-page (planner-task-date task-info)))
                (unless task-info
                  (error "There is no task on the current line"))
                (unless force
                  (when (equal date-page date)
                    (error "Cannot move a task back to the same day"))
                  (when (equal (planner-task-status task-info) "X")
                    (error "Cannot reschedule a completed task"))
                  (when (equal (planner-task-status task-info) "C")
                    (error "Cannot reschedule a cancelled task")))
                (when (and (or (null date) (string= date "nil"))
                           (not plan-page))
                  (error "Cannot unset date in task not associated with plan"))
                ;; Delete it from the old date page
                (when date-page
                  (planner-goto date-page)
                  (goto-char (point-min))
                  (when (planner-find-task task-info)
                    (beginning-of-line)
                    (delete-region (point)
                                   (min (point-max)
                                        (1+ (planner-line-end-position))))))
                ;; Update the new date page
                (unless (null date)
                  (planner-goto date)
                  (when (or (not planner-copy-or-move-task-suppress-duplicates)
                            (and (not (planner-find-task task-info))))
                    (planner-seek-task-creation-point)
                    (insert
                     (planner-format-task task-info
                                          nil nil nil nil
                                          (when plan-page
                                            (planner-make-link plan-page)))
                     "\n")))
                ;; Update planner page
                (when (and plan-page
                           (not (string-match planner-date-regexp plan-page)))
                  (planner-find-file plan-page)
                  (goto-char (point-min))
                  (if (planner-find-task task-info)
                      (progn
                        (beginning-of-line)
                        (delete-region (point)
                                       (min (point-max)
                                            (1+ (planner-line-end-position)))))
                    (planner-seek-task-creation-point))
                  (insert (planner-format-task
                           task-info
                           nil nil nil nil
			   (planner-make-link date)) "\n"))
                t))))
        ;; Operation successful.
        (when planner-tasks-file-behavior
          (planner-save-buffers live-buffers t)))
    (when (planner-replan-task date) t)))
(defalias 'planner-copy-or-move-task 'planner-copy-or-move-task-basic)

;;;_  + Deleting

(defun planner-delete-task ()
  "Deletes this task from the current page and the linked page."
  (interactive)
  (save-excursion
    (save-window-excursion
      (beginning-of-line)
      (let* ((task-info (planner-current-task-info))
             (task-link (and task-info (planner-task-link task-info)))
             (live-buffers
              (and (equal planner-tasks-file-behavior 'close)
                   (buffer-list))))
        (unless task-info
          (error "There is no task on the current line"))
        (beginning-of-line)
        (delete-region (point) (min (point-max)
                                    (1+ (planner-line-end-position))))
        (when (and task-link (assoc task-link (planner-file-alist)))
          (planner-jump-to-linked-task task-info)
          (delete-region (planner-line-beginning-position)
                         (min (point-max) (1+ (planner-line-end-position)))))
        (when planner-tasks-file-behavior
          (planner-save-buffers live-buffers t))))))

;;;_  + Updating

(defun planner-edit-task-description-basic (description)
  "Change the current task to use DESCRIPTION."
  (interactive (list
                (let* ((info (planner-current-task-info))
                       (planner-task-history
                        (list
                         (planner-task-description info))))
                  (unless info (error "No task on current line"))
                  (read-string "New description: "
                               (cons (planner-task-description info)
                                     1)
                               '(planner-task-history . 1)
                               (planner-task-description info)))))
  (let ((point (point)))
    (with-planner-update-setup
     (let ((info (planner-current-task-info))
           (live-buffers (and
                          (equal planner-tasks-file-behavior 'close)
                          (buffer-list))))
       (delete-region (planner-line-beginning-position)
                      (planner-line-end-position))
       (insert (planner-format-task info
                                    nil nil nil
                                    description))
       (when (planner-task-link info)
         (if (planner-jump-to-linked-task info)
             (progn
               (setq info (planner-current-task-info))
               (delete-region (planner-line-beginning-position)
                              (planner-line-end-position))
               (insert (planner-format-task info
                                            nil nil nil
                                            description)))
           (planner-seek-task-creation-point)
           (insert
            (planner-format-task info nil nil nil nil
                                 (planner-make-link (planner-task-page info)))
            "\n")))))
    (goto-char (point))))
(defalias 'planner-edit-task-description 'planner-edit-task-description-basic)

;; Use a macro for the setup around planner-update-task so
;; the same setup can be used in planner-multi.el
(defmacro with-planner-update-setup (&rest body)
  "Execute BODY then save buffers according to `planner-tasks-file-behavior'.
Also sets some variables to modify font-lock behaviour while updating."
  (let ((live-buffers (make-symbol "live-buffers")))
    `(save-window-excursion
       (save-excursion
         (save-restriction
           (let ((,live-buffers (and (eq planner-tasks-file-behavior
                                         'close)
                                     (buffer-list)))
                 (current-buffer (current-buffer)))
             (prog1
                 (let ((planner-tasks-file-behavior nil))
                   ,@body)
               (when planner-tasks-file-behavior
                 (planner-save-buffers ,live-buffers t current-buffer)))))))))

;; Manually expanded def-edebug-spec so that we don't have to pull
;; edebug in
(put 'with-planner-update-setup 'edebug-form-spec '(body))

(defun planner-update-task-basic ()
  "Update the current task's priority and status on the linked page.
Tasks are considered the same if they have the same description.
This function allows you to force a task to be recreated if it
disappeared from the associated page.

Note that the text of the task must not change.  If you want to be able
to update the task description, see planner-id.el."
  (interactive)
  (with-planner-update-setup
   (beginning-of-line)
   (let* ((task-info (planner-current-task-info))
          (task-link (and task-info
                          (if (string-match planner-date-regexp
                                            (planner-page-name))
                              (planner-task-plan task-info)
                            (planner-task-date task-info))))
          (original (planner-page-name)))
     (unless task-info
       (error "There is no task on the current line"))
     ;;        (unless task-link
     ;;          (error "There is no link for the current task"))
     (if (planner-jump-to-linked-task task-info)
         ;; Already there, so update only if changed
         (unless (planner-tasks-equal-p task-info
                                        (planner-current-task-info))
           (delete-region (planner-line-beginning-position)
                          (min (point-max) (1+ (planner-line-end-position))))
           (insert (planner-format-task task-info nil nil nil nil
                                        (planner-make-link
                                         original)) "\n"))
       ;; Not yet there, so add it
       (when (planner-local-page-p task-link)
         (planner-find-file task-link)
         (save-excursion
           (save-restriction
             (planner-seek-task-creation-point)
             (insert
              (planner-format-task task-info nil nil nil nil
                                   (planner-make-link original))
              "\n"))))))))

(defalias 'planner-update-task 'planner-update-task-basic)

;;;_  + Prioritizing

;; This really should be called planner-raise/lower-task-priority, but
;; for some obscure reason, the original planner.el called the task
;; numbers priorities and "A/B/C" categories. I'm not really sure if I
;; can change the name right now. I suppose we eventually should.

(defun planner-set-task-priority (priority)
  "Set the priority of the current task.
This changes a low-priority task to a medium-priority task
and a medium-priority task to a high-priority task."
  (let ((info (planner-current-task-info)))
    (when info
      (delete-region (planner-line-beginning-position)
                     (min (point-max) (1+ (planner-line-end-position))))
      (save-excursion
        (insert (planner-format-task
                 info
                 priority) "\n"))
      (when (planner-task-link info)
        (planner-update-task)))))

(defun planner-raise-task-priority ()
  "Raise the priority of the current task.
This changes a low-priority task to a medium-priority task
and a medium-priority task to a high-priority task."
  (interactive)
  (let ((info (planner-current-task-info)))
    (when info
      (delete-region (planner-line-beginning-position)
                     (min (point-max) (1+ (planner-line-end-position))))
      (save-excursion
        (insert (planner-format-task
                 info
                 (cond
                  ((string= "A" (planner-task-priority info)) "A")
                  ((string= "B" (planner-task-priority info)) "A")
                  ((string= "C" (planner-task-priority info)) "B")
                  (t "C"))) "\n"))
      (when (planner-task-link info)
        (planner-update-task)))))

(defun planner-lower-task-priority ()
  "Lower the priority of the current task.
This changes a medium-priority task to a low-priority task
and a high-priority task to a low-priority task."
  (interactive)
  (let ((info (planner-current-task-info)))
    (when info
      (delete-region (planner-line-beginning-position)
                     (min (point-max) (1+ (planner-line-end-position))))
      (save-excursion
        (insert (planner-format-task
                 info
                 (cond
                  ((string= "A" (planner-task-priority info)) "B")
                  ((string= "B" (planner-task-priority info)) "C")
                  (t "C"))) "\n"))
      (when (planner-task-link info)
        (planner-update-task)))))

(defun planner-raise-task (&optional arg)
  "Raise the number of the current task by ARG steps. (Default: 1)"
  (interactive "p")
  (beginning-of-line)
  (setq arg (or arg 1)) ; ARG defaults to 1 if not specified
  (if (< arg 0) (planner-lower-task (- arg)))
  (let* ((current-task (planner-current-task-info))
         ;; task-seen will be the last task moved over with the same link
         task-seen)
    (unless current-task
      (error "Not on a task line"))
    ;; Store the current line in the kill ring, deleting it
    (kill-region (point) (1+ (planner-line-end-position)))
    ;; If the previous line is not a task, search for the previous block
    (while (> arg 0)
      (let ((old-point (point)))
        (if (= (forward-line -1) 0)
            (if (not (planner-current-task-info))
                (if (re-search-backward "^#[ABC][0-9]*[ \t]" nil t)
                    (beginning-of-line)
                  (setq arg -1) ;; Stop moving, yank back at current place.
                  (goto-char old-point)))
          (setq arg -1)) ;; Stop moving, yank back at current place.
        (when (and (> arg 0)
                   (string= (planner-task-plan current-task)
                            (planner-task-plan (planner-current-task-info))))
          (setq task-seen (planner-current-task-info))))
      (setq arg (1- arg)))
    ;; Cursor now at right place
    (save-excursion (yank))
    ;; Update the linked page, if any
    (save-window-excursion
      (save-excursion
        (save-restriction
          (when (and task-seen
                     (planner-task-link current-task)
                     (planner-jump-to-linked-task current-task))
            (let ((old-task
                   (buffer-substring
                    (planner-line-beginning-position)
                    (planner-line-end-position)))
                  found)
              (save-excursion
                (save-restriction
                  (when (planner-find-task task-seen)
                    ;; Found the new task, so delete the old task and insert here
                    (setq found t)
                    (insert old-task "\n"))))
              (when found
                (delete-region
                 (planner-line-beginning-position)
                 (1+ (planner-line-end-position)))))))))))

(defun planner-lower-task (&optional arg)
  "Lower the number of the current task by ARG steps (default 1)."
  (interactive "p")
  (beginning-of-line)
  (setq arg (or arg 1)) ; ARG defaults to 1 if not specified
  (if (< arg 0) (planner-raise-task (- arg)))
  (let* ((current-task (planner-current-task-info))
         ;; task-seen will be the last task moved over with the same link
         task-seen)
    (unless current-task
      (error "Not on a task line"))
    ;; Store the current line in the kill ring, deleting it
    (kill-region (point) (1+ (planner-line-end-position)))
    ;; If the current line is not a task, search for the next block
    (while (> arg 0)
      (let ((old-point (point)))
        (if (not (planner-current-task-info))
            (if (re-search-forward "^#[ABC][0-9]*[ \t]" nil t)
                (planner-line-beginning-position)
              (setq arg -1) ;; Stop moving, yank back at current place.
              (goto-char old-point)))
        (when (and (> arg 0)
                   (string= (planner-task-plan current-task)
                            (planner-task-plan (planner-current-task-info))))
          (setq task-seen (planner-current-task-info))))
      (unless (and (> arg 0) (= (forward-line 1) 0))
        (setq arg -1))
      (setq arg (1- arg)))
    ;; Cursor now at right place
    (save-excursion (yank))
    ;; Update the linked page, if any
    (save-window-excursion
      (save-excursion
        (save-restriction
          (when (and task-seen
                     (planner-task-link current-task)
                     (planner-jump-to-linked-task current-task))
            (let ((old-task
                   (buffer-substring
                    (planner-line-beginning-position)
                    (planner-line-end-position)))
                  found)
              (save-excursion
                (save-restriction
                  (when (planner-find-task task-seen)
                    ;; Found the new task, so delete the old task and insert here
                    (setq found t)
                    (forward-line 1)
                    (insert old-task "\n"))))
              (when found
                (delete-region
                 (planner-line-beginning-position)
                 (1+ (planner-line-end-position)))))))))))

;;;_  + Changing the status

(defvar planner-mark-task-hook nil
  "Hook called after a task's status has been changed.
Arguments are OLD-STATUS and NEW-STATUS. Functions should leave
the point on the task. If a function returns nil, no other
functions will be processed.")

(defun planner-mark-task (mark &optional this-only)
  "Change task status to MARK.
If THIS-ONLY is non-nil, the linked planner page is not
updated."
  (let ((case-fold-search nil)
        (info (planner-current-task-info)))
    (when info
      (with-planner-update-setup
       (goto-char (planner-line-beginning-position))
       (skip-chars-forward "^ \t" (planner-line-end-position))
       (skip-chars-forward " \t" (planner-line-end-position))
       (delete-char 1)
       (insert mark)
       (unless this-only
         (planner-update-task))
       (run-hook-with-args-until-failure
        'planner-mark-task-hook
        (planner-task-status info)
        mark)))))

(defun planner-task-open ()
  "Mark the current task as open."
  (interactive)
  (planner-mark-task "_"))

(defun planner-task-in-progress ()
  "Mark the current task as in progress."
  (interactive)
  (planner-mark-task "o"))

(defun planner-task-done ()
  "Mark the current task as done."
  (interactive)
  (planner-mark-task "X"))

(defun planner-task-cancelled ()
  "Mark the current task as cancelled."
  (interactive)
  (planner-mark-task "C"))

(defun planner-task-delegated ()
  "Mark the current task as delegated."
  (interactive)
  (planner-mark-task ">"))

(defun planner-task-pending ()
  "Mark the current task as pending."
  (interactive)
  (planner-mark-task "P"))

;;;_  + Extracting

(defun planner-seek-next-unfinished-task ()
  "Move point to the next unfinished task on this page.
Return nil if not found."
  (interactive)
  (re-search-forward "^#[A-C][0-9]*\\s-+[_o>P]\\s-+" nil t))

(defun planner-list-tasks-with-status (status &optional check-plan-pages)
  "Display all tasks that match the STATUS regular expression on all day pages.
If CHECK-PLAN-PAGES is non-nil, plan pages are also checked.  This
could take a long time."
  (interactive (list (read-string "Status: ")))
  (set-buffer (get-buffer-create "*Planner Tasks*"))
  (erase-buffer)
  (let (tasks)
    (setq tasks (planner-extract-tasks
		 (if check-plan-pages
		     (planner-file-alist)
		   (planner-get-day-pages))
		 (lambda (item)
		   (string-match status (planner-task-status item)))))
    (while tasks
      (insert
       (format "[[%s]] %s %s %s\n"
	       (planner-task-page (car tasks))
	       (planner-task-priority (car tasks))
	       (planner-task-status (car tasks))
	       (planner-task-description (car tasks))))
      (setq tasks (cdr tasks))))
  (planner-mode)
  (goto-char (point-min))
  (pop-to-buffer (current-buffer)))

(defun planner-list-unfinished-tasks (&optional pages)
  "Display all unfinished tasks on PAGES.
The PAGES argument limits the pages to be checked in this manner:
  t: check all pages
  \"regexp\": search all pages whose filenames match \"regexp\"
  list of page names: limit to those pages
  alist of page/filenames: limit to those pages

Called interactively, this function will search day pages by
default. You can specify the start and end dates or leave them as
nil to search all days. Calling this function with an interactive
prefix will prompt for a regular expression to limit pages.
Specify \".\" or leave this blank to include all pages."
  (interactive (list (if current-prefix-arg
                         (read-string "Regexp: ")
                       (let ((planner-expand-name-default "nil"))
                         (planner-get-day-pages
                          (planner-read-date "nil by default. Start")
                          (planner-read-date "nil by default. End")
                          t)))))
  (planner-list-tasks-with-status "[^XC]" pages))

;;;_ + Notes

(defvar planner-search-notes-buffer "*Planner Search*"
  "Buffer for search results.")

;;;###autoload
(defun planner-search-notes-with-body (regexp limit)
  "Return a buffer with all the notes returned by the query for REGEXP.
If called with a prefix argument, prompt for LIMIT and search days on
or after LIMIT. Display the body of the notes as well."
  (interactive (list (read-string "Regexp: ")
                     (if current-prefix-arg
                         (let ((planner-expand-name-favor-future-p nil))
                           (planner-read-date)))))
  (planner-search-notes regexp limit t))

;;;###autoload
(defun planner-search-notes (regexp limit &optional include-body)
  "Return a buffer with all the notes returned by the query for REGEXP.
If called with a prefix argument, prompt for LIMIT and search days on
or after LIMIT. If INCLUDE-BODY is non-nil, return the body as well."
  (interactive (list (read-string "Regexp: ")
                     (if current-prefix-arg
                         (let ((planner-expand-name-favor-future-p nil))
                           (planner-read-date)))
                     nil))
  (with-planner
    (let* ((case-fold-search t)
           (results (planner-search-notes-internal regexp limit include-body))
           (emacs-wiki-project planner-project))
      (if results
          (progn
            (set-buffer (get-buffer-create planner-search-notes-buffer))
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq emacs-wiki-current-project planner-project)
            (mapcar
             (if include-body
                 (lambda (item)
                   (insert "** "
                           (planner-make-link (elt item 0) nil t) "\t"
                           (elt item 2) "\n\n"))
               (lambda (item)
                 (insert (planner-make-link (car item) nil t) "\t"
                         (cadr item) "\n")))
             results)
            (planner-mode)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer)))
        (message "No results found.")))))

;;;_ + Calendar

(defun planner-calendar-insinuate ()
  "Hook Planner into Calendar.

Adds special planner key bindings to `calendar-mode-map'.
After this function is evaluated, you can use the following
planner-related keybindings in `calendar-mode-map':

   n     jump to the planner page for the current day.
   N     display the planner page for the current day."
  (interactive)
  (require 'calendar)
  (add-hook 'calendar-move-hook
            (lambda ()
              (when planner-calendar-show-planner-files
                (planner-calendar-show))))
  (define-key calendar-mode-map "n" 'planner-calendar-goto)
  (define-key calendar-mode-map "N" 'planner-calendar-show))
(defalias 'planner-insinuate-calendar 'planner-calendar-insinuate)

(defvar planner-calendar-buffer-list nil "List of buffers opened by calendar.")

(defun planner-kill-calendar-files ()
  "Remove planner files shown from Calendar."
  (interactive)
  (while planner-calendar-buffer-list
    (when (buffer-live-p (car planner-calendar-buffer-list))
      (with-current-buffer (car planner-calendar-buffer-list)
        (save-buffer)
        (planner-maybe-remove-file)))
    (setq planner-calendar-buffer-list (cdr planner-calendar-buffer-list))))

;;;###autoload
(defun planner-calendar-goto ()
  "Goto the plan page corresponding to the calendar date."
  (interactive)
  (let ((planner-use-other-window t))
    (planner-goto (planner-date-to-filename (calendar-cursor-to-date)))))

;;;###autoload
(defun planner-calendar-show ()
  "Show the plan page for the calendar date under point in another window."
  (interactive)
  (save-selected-window
    (let ((planner-use-other-window t)
          (date (planner-date-to-filename (calendar-cursor-to-date))))
      (if (planner-goto date planner-show-only-existing)
          (add-to-list 'planner-calendar-buffer-list (current-buffer))
        ;; Return nil or a message if there is no day plan page.  planner-goto
        ;; is not called interactively, so it doesn't send a message.
        (when (interactive-p)
          (message "No planner file for %s" date))
        ;; return nil
        nil))))

(defadvice exit-calendar (after planner activate protect compile)
  "Call `planner-kill-calendar-files'."
  (planner-kill-calendar-files))

(defun planner-calendar-select ()
  "Return to `planner-read-date' with the date currently selected."
  (interactive)
  (when (calendar-cursor-to-date)
    (setq planner-calendar-selected-date
          (planner-date-to-filename (calendar-cursor-to-date)))
    (if (active-minibuffer-window) (exit-minibuffer))))

;;;_* Context-sensitive keybindings

(defun planner-jump-to-link ()
  "Jump to the item linked to by the current item."
  (interactive)
  (cond
   ((planner-current-task-info) (planner-jump-to-linked-task))
   ((planner-current-note-info) (planner-jump-to-linked-note))))

(defun planner-move-up ()
  "Move up.
Task: Raise the number of the current task.
Note: Renumbering does not make sense for notes right now, so go to the
previous note.
Headline: Go to previous headline of the same depth."
  (interactive)
  (cond
   ((planner-current-task-info) (planner-raise-task))
   ((planner-current-note-info)
    (re-search-backward "^\\.#[0-9]+" nil t))
   ((and (goto-char (planner-line-beginning-position))
         (looking-at "^\\*+"))
    (re-search-backward
     (concat "^" (regexp-quote (match-string 0)) "\\s-") nil t))))


(defun planner-move-down ()
  "Move down.
Task: Lower the number of the current task.
Note: Renumbering does not make sense for notes right now, so go to the
next note.
Headline: Go to the next headline of the same depth."
  (interactive)
  (cond
   ((planner-current-task-info) (planner-lower-task))
   ((planner-current-note-info)
    (forward-line 1)
    (re-search-forward "^\\.#[0-9]+" nil t))
   ((and (goto-char (planner-line-beginning-position))
         (looking-at "^\\*+"))
    (forward-line 1)
    (re-search-forward
     (concat "^" (regexp-quote (match-string 0)) "\\s-") nil t))))

;;;_* Initialization

(setq planner-loaded t)
(planner-update-wiki-project)
(add-hook 'emacs-wiki-update-project-hook 'planner-update-wiki-project)
(put 'planner-mode 'flyspell-mode-predicate 'emacs-wiki-mode-flyspell-verify)

(provide 'planner)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; planner.el ends here
