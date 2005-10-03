;;; planner-rank.el --- Importance vs Urgency for the Emacs planner
;;

;; Keywords: emacs planner important emergent rank deadline
;; Author: Dryice Liu <dryice AT liu DOT com DOT cn>
;; Description: calculate task rank from importance and urgency

;; This file is not part of GNU Emacs.

;; Copyright (C) 2005 Dryice Dong Liu . All rights reserved.

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
;; This file models Franklin Covey's Urgency and Importance
;; principle. Where for both Urgency and Importance, you can measure
;; them with number 0-9. 9 means "I should have done this from
;; Emergency/Important point of view" while 0 means "I can forget this
;; from Emergency/Important point of view".
;;
;; If you use the planner-deadline feature, the Urgency value is
;; calculated from how many days left to meet the deadline. You can
;; manipulate `planner-rank-deadline-urgency-map-list' if you have
;; speal needs.
;;
;; The rank is calculated from Urgency and Importance. You can choose
;; your favoriate algorithm by setting
;; `planner-rank-rank-calculate-function'. Took a look at the
;; planner-rank-calculate-rank-* functions to see which one you like
;; most. Call `planner-rank-test-algorithm' to get the result tables
;; and see how it works.
;; 
;; The three planner-sort-tasks-by-* functions in this file is
;; suitable for `planner-sort-tasks-key-function'.
;;
;; if you want to set rank for every new task, add
;;
;; (add-hook 'planner-create-task-from-buffer-hook 'planner-rank-change)
;; 
;; to your .emacs.
;;
;;; TODO
;; - planner-timeclock-summary.el integration
;;
;;; Code:

(require 'planner)
(require 'planner-deadline)

;;; USER VARIABLES -----------------------------------------------------------

(defgroup planner-rank nil
  "Importance vs Urgency support for planner.el."
  :prefix "planner-rank"
  :group 'planner)

(defcustom planner-rank-change-hook nil
  "Functions to run after `planner-rank-change'.
Point will be on the same line as the task."
  :type 'hook
  :group 'planner-rank)

(defcustom planner-rank-priority-A-valve 6
  "\"A\" \"B\" task valve.
Tasks with rank greater than or equal to this value will be in
priority A."
  :type 'number
  :group 'planner-rank)

(defcustom planner-rank-priority-B-valve 3
  "\"B\" \"C\" task valve.
Tasks with rank greater than or equal to this value and less than
`planner-rank-priority-A-valve' will be in priority B. Tasks with
rank less than this value will be in priority C."
  :type 'number
  :group 'planner-rank)

(defcustom planner-rank-deadline-urgency-map-list
  '(-1 0 3 7 14 21 30 90 365)
  "Deadline to Urgency map.
If there is a deadline for the task, the urgency value is
calculated from how many days left and this list. E.g, there's n
days left, if n is less or equal than nth 0 of this list, it will
get urgency 9. If it's greater than nth 0, but less or equal than
nth 1, it will be urgency 8. It's the user's responsibility to
keep the list in order.

The meaning of the default value:
+-------------+---------+
|Days left    |Urgency  |
+-------------+---------+
|overdue      |9        |
|today        |8        |
|3 days       |7        |
|a week       |6        |
|2 weeks      |5        |
|3 weeks      |4        |
|this month   |3        |
|this quarter |2        |
|this year    |1        |
|longer       |0        |
+-------------+---------+"
  :type '(repeat integer)
  :group 'planner-rank)

(defcustom planner-rank-default-importance 5
  "Default importance value for newly added rank."
  :type 'integer
  :group 'planner-rank)

(defcustom planner-rank-default-urgency 5
  "Default urgency value for newly added rank."
  :type 'integer
  :group 'planner-rank)

(defcustom planner-rank-importance-vs-urgency-factor 1.5
  "The weight of importance vs urgency.
How much do you think importance is more \"important\" than
urgency. This will be used in
planner-rank-calculate-rank-weighted-* functions."
  :type 'number
  :group 'planner-rank)

(defcustom planner-rank-rank-calculate-function
  'planner-rank-calculate-rank-weighted-rmsd
  "Define the function called to calculate rank.
The function should take two arguments: the importance and the
urgency value, and return the calculated rank value."
  :type 'function
  :group 'planner-rank)

(defcustom planner-rank-test-buffer "*Planner Rank Test Result*"
  "Buffer name for timeclock reports from `planner-rank-test-algorithm'."
  :type 'string
  :group 'planner-rank)

;;;_+ Internal variables and utility functions

(add-hook 'planner-deadline-change-hook 'planner-rank-update-current-task)

(defconst planner-rank-regexp
"\\(\\s-*{{Rank:\\s-+\\([0-9]\\.[0-9]+\\)\\s-+-\\s-+I=\\([0-9]\\)\\s-+U=\\([0-9]\\)}}\\)"
  "Regular expression for rank data.
Regexp group 1 is the whole rank string.
Regexp group 2 is the rank value.
Regexp group 3 is the importance value.
Regexp group 4 is the urgency value.")

(defun planner-rank-get-task-info ()
  "Get the rank of the current task.
Return nil if there's no rank string in the current task."
  (save-excursion
    (let ((string (buffer-substring (planner-line-beginning-position)
                                    (planner-line-end-position))))
      (when (string-match planner-rank-regexp string)
        (list (string-to-number (planner-match-string-no-properties 2 string))
              (string-to-int (planner-match-string-no-properties 3 string))
              (string-to-int
               (planner-match-string-no-properties 4 string)))))))

(defsubst planner-rank-rank (info)
  "Return the rank of a task given INFO." (nth 0 info))
(defsubst planner-rank-importance (info)
  "Return the importance of a task given INFO." (nth 1 info))
(defsubst planner-rank-urgency (info)
  "Return the urgency of a task given INFO." (nth 2 info))

(defun planner-rank-calculate-base (arg)
  "Get the base of importance or urgency.
ARG is the basic priority."
  (cond
   ((>= arg planner-rank-priority-A-valve)
    planner-rank-priority-A-valve)
   ((>= arg planner-rank-priority-B-valve)
    planner-rank-priority-B-valve)
   (t 0)))

(defun planner-rank-calculate-rank-average (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The algorithm is a simple average.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-average\)"
  (/ (+ importance urgency) 2.0))

(defun planner-rank-calculate-rank-average-aggressive (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The algorithm is a simple average. Plus making sure the result is
close to the higher value.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-average-aggressive\)"
  (let ((average (/ (+ importance urgency) 2.0)))
    (max average
	 (planner-rank-calculate-base importance)
	 (planner-rank-calculate-base urgency))))

(defun planner-rank-calculate-rmsd (a b)
  "Return the root mean square deviation of A and B."
  (sqrt (/ (+ (* a a) (* b b)) 2.0)))

(defun planner-rank-calculate-rank-rmsd (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The algorithm is the root mean square deviation of importance and
urgency. Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-rmsd\)"
  (planner-rank-calculate-rmsd importance urgency))

(defun planner-rank-calculate-rank-rmsd-aggressive (importance
							urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The algorithm is the RMDS first, if it's smaller than the base of the
bigger arg, return sqrt(RMDS) + base.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-rmsd-aggressive\)"
  (let ((base (planner-rank-calculate-base
	       (max importance urgency)))
	(rmsd (planner-rank-calculate-rmsd importance urgency)))
    (if (< rmsd base)
	(+ base (sqrt rmsd))
      rmsd)))

(defun planner-rank-calculate-rank-aggressive-rmsd-rest
  (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

First we make sure the result is bigger than base, then we add
 (sqrt(rmsd rest smaller)).
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-aggressive-rmsd-rest\)"
  (let* ((bigger (max importance urgency))
	 (smaller (min importance urgency))
	 (base (planner-rank-calculate-base bigger)))
    (+ base (sqrt (planner-rank-calculate-rmsd smaller
						 (- bigger base))))))

(defun planner-rank-calculate-rank-aggressive-rmsd-full
  (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

First we make sure the result is bigger than base, then we add
 (sqrt(rmsd bigger smaller)).
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-aggressive-rmsd-full\)"
  (let* ((bigger (max importance urgency))
	 (smaller (min importance urgency))
	 (base (planner-rank-calculate-base bigger)))
    (+ base (sqrt (planner-rank-calculate-rmsd smaller bigger)))))

(defun planner-rank-calculate-rank-average-base-rmsd (importance
							  urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The average of two base plus rmsd.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-average-base-rmsd\)"
  (+ (/ (+ (planner-rank-calculate-base importance)
	   (planner-rank-calculate-base urgency))
	2.0)
     (sqrt (planner-rank-calculate-rmsd importance urgency))))

(defun planner-rank-calculate-rank-weighted-average (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The weighted average.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-weighted-average\)"
  (/ (+ (* importance planner-rank-importance-vs-urgency-factor)
	urgency)
     (+ planner-rank-importance-vs-urgency-factor 1.0)))

(defun planner-rank-calculate-rank-weighted-rmsd (importance urgency)
  "Calculate the rank from IMPORTANCE and URGENCY.

The weighted rmsd.
Eval the following sexp to see how it works:

\(planner-rank-test-algorithm 'planner-rank-calculate-rank-weighted-rmsd\)"
  (sqrt (/ (+ (* planner-rank-importance-vs-urgency-factor
		 planner-rank-importance-vs-urgency-factor
		 importance
		 importance)
	      (* urgency urgency))
	   (+ (* planner-rank-importance-vs-urgency-factor
		 planner-rank-importance-vs-urgency-factor)
	      1.0))))

(defun planner-rank-calculate-urgency-from-deadline (days)
  "Calculate urgency from how many DAYS are left."
  (let ((list planner-rank-deadline-urgency-map-list)
        value)
    (while list
      (if (<= days (car list))
          (setq value (prog1 (length list)
            (setq list nil)))
        (setq list (cdr list))))
    value))

(defun planner-rank-calculate-rank (importance urgency)
  "Make sure the rank is not greater than 9, just in case.
This is based on IMPORTANCE and URGENCY."
  (min (funcall planner-rank-rank-calculate-function
                importance urgency)
       9))


(defun planner-rank-calculate-priority-from-rank (rank)
  "Calculate task priority (A,B,C) from RANK."
  (cond
   ((>= rank planner-rank-priority-A-valve) "A")
   ((>= rank planner-rank-priority-B-valve) "B")
   (t "C")))

(defun planner-rank-read-importance-and-urgency ()
  "Read importance and urgency.
Use old value as default if available."
  (let ((rank-info (planner-rank-get-task-info))
	(task-info (planner-current-task-info))
	(old-importance planner-rank-default-importance)
	(old-urgency planner-rank-default-urgency)
	(deadline (planner-deadline-get-current-deadline))
	new-importance new-urgency)
    (if rank-info
	(progn
	  (setq old-importance (planner-rank-importance rank-info))
	  (setq old-urgency (planner-rank-urgency rank-info))))
    (setq new-importance
          (string-to-number (read-string "Importance: " nil nil (number-to-string old-importance))))
    (if deadline
	(setq new-urgency
	      (planner-rank-calculate-urgency-from-deadline
	       (planner-deadline-days-left deadline task-info)))
      (setq new-urgency
            (string-to-number (read-string "Urgency: " nil nil (number-to-string old-urgency)))))
    (list new-importance new-urgency)))

;;;###autoload
(defun planner-sort-tasks-by-rank ()
  "Sort tasks by status (_P>XC), priority (ABC), and rank.
Suitable for `planner-sort-tasks-key-function'"
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
           ((eq ch ?C) 300))
     (if (planner-rank-rank (planner-rank-get-task-info))
	 (* 10 (- 10 (planner-rank-rank
		      (planner-rank-get-task-info))))
       99))))

;;;###autoload
(defun planner-sort-tasks-by-importance ()
  "Sort tasks by status (_P>XC), priority (ABC), and importance.
Suitable for `planner-sort-tasks-key-function'"
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
           ((eq ch ?C) 300))
     (if (planner-rank-importance (planner-rank-get-task-info))
	 (* 10 (- 10 (planner-rank-importance (planner-rank-get-task-info))))
       99))))

;;;###autoload
(defun planner-sort-tasks-by-urgency ()
  "Sort tasks by status (_P>XC), priority (ABC), and urgency.
Suitable for `planner-sort-tasks-key-function'"
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
           ((eq ch ?C) 300))
     (if (planner-rank-urgency (planner-rank-get-task-info))
	 (* 10 (- 10 (planner-rank-urgency (planner-rank-get-task-info))))
       99))))

;; for developer

(defun planner-rank-test-algorithm (test-function)
  "Make a test result table for the given TEST-FUNCTION."
  (interactive "aTest function: ")
  (switch-to-buffer (get-buffer-create planner-rank-test-buffer))
  (erase-buffer)
  (insert (format "Result table for function \`%s\'\n\n\n" test-function))
  (let ((row 0)
        (column 0))
    (insert "     |")
    (while (< column 10)
      (insert (format "%3d  |" column))
      (setq column (1+ column)))
    (insert " -> Importance\n")
    (while (< row 10)
      (insert (format "%4d |" row))
      (setq column 0)
      (while (< column 10)
        (insert (format "%.2f |" (funcall test-function column row)))
        (setq column (1+ column)))
      (insert "\n")
      (setq row (1+ row)))
    (insert "\n   |\n   V\nUrgency")))

;; user visible functions

;;;###autoload
(defun planner-rank-change (&optional importance urgency)
  "Change the IMPORTANCE and URGENCY of the current task.
If there's deadline available, calculate urgency instead of asking
the user."
  (interactive)
  (unless (and importance urgency)
    (let ((temp (planner-rank-read-importance-and-urgency)))
      (setq importance (car temp))
      (setq urgency (cadr temp))))
  (save-excursion
    (save-match-data
      (let* ((info (planner-current-task-info))
	     (old-description (planner-task-description info))
	     (old-priority (planner-task-priority info))
	     (rank (planner-rank-calculate-rank
		    importance urgency))
	     (new-priority
	      (planner-rank-calculate-priority-from-rank rank))
	     new-description)
	(if info
	    (progn
	      (if (not (string= old-priority new-priority))
		  (planner-set-task-priority new-priority))
	      (if (string-match planner-rank-regexp old-description)
		  (setq new-description (replace-match "" t t old-description))
		(setq new-description old-description))
	      (planner-edit-task-description
	       (format "%s {{Rank: %.2f - I=%d U=%d}}"
		       new-description
		       rank
		       importance
		       urgency))
	      (run-hooks 'planner-rank-change-hook)))))))

;;;###autoload
(defun planner-rank-update-current-task ()
  "Re-calculate rank for the current task."
  (interactive)
  (let* ((task-info (save-match-data (planner-current-task-info)))
	 (status (planner-task-status task-info))
	 (deadline (planner-deadline-get-current-deadline))
	 (rank-info (planner-rank-get-task-info))
	 (importance (planner-rank-importance rank-info))
	 (urgency (planner-rank-urgency rank-info)))
    (when (and deadline
               task-info
               (not (equal status "X"))
               (not (equal status "C")))
      (setq urgency (planner-rank-calculate-urgency-from-deadline
                     (planner-deadline-days-left deadline task-info)))
      (planner-rank-change importance urgency))))

;;;###autoload
(defun planner-rank-update-all ()
  "Re-calculate rank for all tasks in the current page."
  (interactive)
  (save-excursion
    (save-restriction
      (when (planner-narrow-to-section 'tasks)
	(goto-char (point-min))
	(while (re-search-forward planner-rank-regexp nil t)
	  (save-match-data
	    (planner-rank-update-current-task))
	  (forward-line 1))))))

(provide 'planner-rank)

;;; planner-rank.el ends here

