;;; planner-trunk.el --- Trunk tasks for the Emacs planner
;;

;; Keywords: emacs planner trunk group tasks
;; Authors: Dryice Liu <dryice AT liu DOT com DOT cn>
;;          Keith Amidon <camalot AT picnicpark dot org>
;; Description: trunk(group) tasks for the Emacs planner

;; This file is not part of GNU Emacs.

;; Copyright (C) 2005 Dryice Dong Liu . All rights reserved.
;; Parts Copyright (C) 2005 Keith Amidon

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
;; This file provides `planner-trunk-tasks', which groups the tasks
;; according to `planner-trunk-rule-list'. Please see the docstring
;; for details. Remember to customize `planner-trunk-rule-list' before
;; trying it out.
;;
;; It sorts and splits your tasks, adding a blank line between groups
;; of tasks.
;;
;; WARNING: planner-trunk will delete *ALL* non-task lines from the
;;          tasks section of your plan page if it attempts to trunk
;;          the tasks. Do NOT use it if you want to preserve this
;;          information.

;;; Things that would be nice to do:
;; - Respect hidden outline sections when trunking and rehide after
;;   trunk is complete if they are present.
;; - If point is in the tasks section, keep point on the same line.
;;   Maybe can do by saving entire line text and searching for it
;;   afterwards.  Only problem is if it is whitespace only line.  If
;;   so, maybe can move cursor to previous non-whitespace line?  Point
;;   obviously shouldn't move if not in Tasks section.

;;; Code:

(require 'planner)

;;; USER VARIABLES -----------------------------------------------------------

(defgroup planner-trunk nil
  "Grouping tasks for planner.el."
  :prefix "planner-trunk"
  :group 'planner)

(defcustom planner-trunk-rule-list  `(("`[0-9][0-9][0-9][0-9].[0-9][0-9].[0-9][0-9]'" nil
                                      ("HomeWork" "WorkStuff"
                                       "EmacsHack\\|PlannerHack")))
  "List of rules for trunking tasks.

Each rule is a sublist of the form:

   (PAGE-REGEXP COMPLETE-HEADING TRUNK-SECTIONS-LIST)

PAGE-REGEXP is used to select the set of trunk sections that should be
used.  It is matched against the name of the current planner page.  If
no matching PAGE-REGEXP is found, no trunking is done.  If there is
more than one match, the first one in the list is used.

If COMPLETE-HEADING is nil, completed and not completed tasks will be
in the same trunk, sorted according to `planner-sort-tasks-key-function'.
If it is a string, it is the name of a sub-heading of the tasks
section under which to sort completed tasks separately, in which
case it will be the last subsection of the tasks section of the page.

Each element of TRUNK-SECTIONS-LIST describes a trunk of the page.
Elements can either be a simple TASK-PLAN-REGEXP, or a sublist of the form:

   (TASK-PLAN-REGEXP TRUNK-HEADING)

The TASK-PLAN-REGEXP is matched against the plan page (or pages if you
are using planner-multi) for the task.  If more than one
TASK-PLAN-REGEXP matches, the first one in the list is used.  All
tasks that match the same TASK-PLAN-REGEXP will be grouped together.
The order of the groups will match the order of TRUNK-SECTIONS-LIST.
Tasks that do not match any entry in TRUNK-SECTIONS-LIST will be in a
separate group at the end of the tasks section.  If the sublist form
of an entry is used, TRUNK-HEADING is a name for the outline heading
to be inserted at the top of the trunk.  If TRUNK-SECTIONS-LIST
contains a mix of items in the regexp and sublist forms, when tasks
are present that match a regexp form entry, but no tasks are present
that match the most recent preceeding sublist form entry in the list,
the heading from the sublist form entry will be inserted anyway.  In
use, it will become obvious why this is desirable."
  :type '(repeat (list
                  :tag "Trunk rule"
                  (choice :tag "Page regexp"
                          (const "`[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]'"
                                 :tag "Day pages")
                          (const "." :tag "All pages")
                          (regexp :tag "Regexp"))
                  (choice
                   :tag "Completed tasks"
                   (const :tag "With incomplete tasks" nil)
                   (string :tag "Under section heading"))
                  (repeat (choice (regexp :tag "Regexp")
                                  (list
                                   :tag "Regexp and section heading"
                                   (regexp :tag "Regexp")
                                   (string :tag "Section heading"))))))
  :group 'planner-trunk)

(defcustom planner-trunk-tasks-before-hook nil
  "Functions to run before doing the trunk."
  :type 'hook
  :group 'planner-trunk)

(defcustom planner-trunk-tasks-after-hook nil
  "Functions to run after the trunk is done."
  :type 'hook
  :group 'planner-trunk)

;;;_+ Internal variables and utility functions

(defun planner-trunk-rule-page-regexp (rule)
  "Regular expression matching the page in RULE."
  (elt rule 0))

(defun planner-trunk-rule-completed-heading (rule)
  "Sub-heading for completed tasks in RULE."
  (elt rule 1))

(defun planner-trunk-rule-trunk-sections (rule)
  "Trunk section in RULE."
  (elt rule 2))

(defun planner-trunk-list-regexp (trunk)
  "Plan page regular expression for TRUNK."
  (if (listp trunk)
      (car trunk)
    trunk))

(defun planner-trunk-list-heading (trunk)
  "Heading for TRUNK."
  (if (listp trunk)
      (cadr trunk)
    nil))

(defun planner-trunk-task-plan-str (task-info)
  "Return plan string for TASK-INFO."
  (or 
   (if (fboundp 'planner-multi-task-link-as-list)
       (mapconcat 'identity
                  (planner-multi-task-link-as-list task-info) " ")
     (or (planner-task-link task-info)
         (planner-task-plan task-info)))
   ""))

(defun planner-trunk-completed-p (task-info)
  "Return non-nil if TASK-INFO is a completed task."
  (or (equal (planner-task-status task-info) "X")
      (equal (planner-task-status task-info) "C")))

(defun planner-trunk-delete-all-blank-lines ()
  "Delete all blank lines and insert one at the end."
  (goto-char (point-min))
  (delete-blank-lines)
  (while (= (forward-line 1) 0)
    (delete-blank-lines))
  (insert "\n"))

(defun planner-trunk-delete-line-if-not-task ()
  "Delete the current line if it is not a task."
  (if (planner-current-task-info)
      (not (equal (forward-line) 1))
    (let ((bol (planner-line-beginning-position)))
      (let ((at-end (equal (forward-line) 1)))
        (beginning-of-line)
        (delete-region bol (point))
        (not at-end)))))

(defun planner-trunk-delete-non-task-lines ()
  "Delete all lines that are not tasks. DANGEROUS."
  (goto-char (point-min))
  (forward-line) ; Skip Tasks heading
  ;; (keep-lines "^#[A-C][0-9]*\\s-+.\\s-") or
  (while (planner-trunk-delete-line-if-not-task))
  (insert "\n")
  (forward-line -1))

(defun planner-trunk-sort-tasks (rule)
  "Sort tasks by plan name according to the given RULE list."
  (let ((trunk-list (planner-trunk-rule-trunk-sections rule))
        (completed-heading (planner-trunk-rule-completed-heading rule))
        (task-info (planner-current-task-info)))
    (let ((trunk-count (length trunk-list))
          (plan (planner-trunk-task-plan-str task-info))
          (task-completed (planner-trunk-completed-p task-info)))
      (if (not plan) 
          (+ 2 (if (and completed-heading task-completed)
                   (* 2 trunk-count)
                 trunk-count))
        (catch 'done
          (let ((count 1))
            (when (and completed-heading task-completed)
              (setq count (+ count trunk-count 2)))
            (mapc
             (lambda (trunk-entry)
              (let ((plan-regexp (planner-trunk-list-regexp trunk-entry)))
                (if (string-match plan-regexp plan)
                    (throw 'done count)
                  (setq count (1+ count))))) 
             trunk-list)
            count))))))

(defun planner-trunk-ins-heading (completed-heading task-info heading)
  "Insert the task heading.
If COMPLETED-HEADING is non-nil and TASK-INFO is a completed task,
use COMPLETED-HEADING instead of HEADING."
  (when heading
    (insert "\n")
    (when (and completed-heading
               (planner-trunk-completed-p task-info))
      (insert "*"))
    (insert "** " heading))
  (insert "\n"))

(defun planner-trunk-do-trunk-section (rule)
  "Really do the trunk.

Adds new lines and optionally outline mode subheadings according to
the trunk RULE.  Point must be at the beginning of the section to
trunk, typically either the beginning of the tasks section or the
beginning of the completed subsection."
  (let ((not-done t)
        (completed-hdr (planner-trunk-rule-completed-heading rule))
        ;; Following adds a dummy first entry to get rid of special
        ;; case to handle headings otherwise.  It prevents anyone from
        ;; having a plan page named (_-), which I hope no-one wants to
        ;; do...
        (trunk-list (cons "^\\\\(_-\\\\)$" 
                          (planner-trunk-rule-trunk-sections rule)))
        (first-trunk (car (planner-trunk-rule-trunk-sections rule)))
        (ntasks 0))
    (while (and trunk-list not-done)
      (let ((task-info (planner-current-task-info)))
        (when task-info
          (setq ntasks (1+ ntasks))
          (let ((plan (planner-trunk-task-plan-str task-info))
                (plan-regexp (planner-trunk-list-regexp (car trunk-list))))
            (unless (string-match plan-regexp plan)
              (let ((hdr nil))
                (while (and trunk-list
                            (not (string-match plan-regexp plan)))
                  (setq trunk-list (cdr trunk-list))
                  (setq plan-regexp
                        (planner-trunk-list-regexp (car trunk-list)))
                  (when (planner-trunk-list-heading (car trunk-list))
                    (setq hdr 
                          (planner-trunk-list-heading (car trunk-list)))))
                (when (planner-trunk-list-heading (car trunk-list))
                  (setq hdr (planner-trunk-list-heading (car trunk-list))))
                (planner-trunk-ins-heading completed-hdr task-info hdr)))))
        (when (or (null trunk-list)
                  (not (equal 0 (forward-line 1)))
                  (and completed-hdr
                       (not (planner-trunk-completed-p task-info))
                       (planner-trunk-completed-p (planner-current-task-info))))
          (setq not-done nil))))
    ntasks))

(defun planner-trunk-do-trunk (rule)
  "Really do the trunk following RULE."
  (goto-char (point-min))
  (planner-trunk-do-trunk-section rule)
  (when (planner-trunk-rule-completed-heading rule)
    (while (let ((task-info (planner-current-task-info)))
             (and (not (planner-trunk-completed-p task-info))
                  (equal 0 (forward-line)))))
    (let ((start-completed-pos (point)))
      (when (> (planner-trunk-do-trunk-section rule) 0)
        (when (stringp (planner-trunk-rule-completed-heading rule))
          (goto-char start-completed-pos)
          (insert "\n** " (planner-trunk-rule-completed-heading rule) "\n"))))))

;; user visible functions

;;;###autoload
(defun planner-trunk-tasks (&optional force)
  "Trunk(group) tasks in the current page.
Please refer the docstring of `planner-trunk-rule-list' for how
it works. You may want to call this function before you sort tasks
and/or after you create new tasks. If a prefix is given or FORCE is not
nil, trunk completed tasks together with non-completed tasks not
matter what the `planner-trunk-rule-list' said."
  (interactive "P")
  (let ((page-name (planner-page-name))
        (rule-list planner-trunk-rule-list))
    (let ((rule (catch 'done
                  (while rule-list
                    (if (string-match (caar rule-list) page-name)
                        (throw 'done (car rule-list))
                      (setq rule-list (cdr rule-list))))
                  nil)))
      (if rule
          (save-excursion
            (save-restriction
	      (run-hooks 'planner-trunk-tasks-before-hook)
              (when (planner-narrow-to-section "Tasks")
                (planner-trunk-delete-non-task-lines)
		(if force
		    (setq rule
			  (list (planner-trunk-rule-page-regexp rule)
				nil
				(planner-trunk-rule-trunk-sections rule))))
                (let ((planner-sort-tasks-key-function
                       (lambda ()
                         (planner-trunk-sort-tasks rule))))
                  (planner-sort-tasks))
                (planner-trunk-do-trunk rule))
	      (run-hooks 'planner-trunk-tasks-after-hook)))))))

(provide 'planner-trunk)

;;; planner-trunk.el ends here

