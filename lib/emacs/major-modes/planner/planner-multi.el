;;; planner-multi.el --- Multiple page support for planner.el

;; Copyright (C) 2004 Sandra Jean Chua (Sacha) <sacha AT free.net.ph>

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

;; TODO: completion that makes sense!

;;; Commentary:
;;
;; After (require 'planner-multi), you should be able to create tasks on multiple
;; pages using M-x planner-create-task-from-buffer or M-x planner-create-task.
;; Notes should also work if you have at least remember--dev--1.0--patch-21.
;;

(require 'planner)
(condition-case err
    (require 'crm)
  ('file-error
   (message "Could not load crm; cumbersome completing read used")))

;;; Code:

(defcustom planner-multi-separator " "
  "String that separates multiple page references.
For best results, this should be something recognized by
`emacs-wiki-link-at-point' so that links are highlighted
separately."
  :type 'string
  :group 'planner)

(defcustom planner-multi-copy-tasks-to-page nil
  "If non-nil, planner page to automatically copy tasks to. Example: TaskPool.
You can specify multiple pages by separating them with `planner-multi-separator',
such as \"[[AllTasksByProject][p]] [[AllTasksByContext][c]]\"."
  :type 'string
  :group 'planner-multi)

;;;_+ Utility functions

(defun planner-multi-next-link (current link-list)
  "Return the item following CURRENT in LINK-LIST."
  (setq current (or current (planner-page-name)))
  (let ((found nil)
        (search link-list))
    (while (and (car search) (null found))
      (when (string= (planner-link-base (car search))
                     current)
        (setq found (car (cdr search))))
      (setq search (cdr search)))
    (or found (car link-list))))

(defun planner-multi-make-link (list)
  "Create a text link for LIST."
  (mapconcat 'planner-make-link
             list
             planner-multi-separator))

(defun planner-multi-link-member (page links)
  "Return non-nil if PAGE is a member of LINKS."
  (let (found)
    (while (and (car links) (null found))
      (when (string= (planner-link-base (car links))
                     page)
        (setq found (car links)))
      (setq links (cdr links)))
    found))

(defun planner-multi-link-delete (page links)
  "Delete PAGE from LINKS."
  (let (results)
    (while links
      (unless (string= (planner-link-base (car links))
                       page)
        (setq results (cons (car links) results)))
      (setq links (cdr links)))
    (nreverse results)))

(defun planner-multi-split (string)
  "Return a list of links in STRING."
  ;; "[[foo bar]] baz [[qux quux]]" should return ("[[foo bar]]" "baz" "[[qux quux]]"), so a simple split-string won't do
  (if (stringp string)
      (let (list
            (pos 0)
            (len (length string)))
        (while (< pos len)
          (if (string-match
               (concat (if (= (aref string pos) ?\[)
                           "\\(\\[\\[.+?\\]\\]\\)"
                         "\\(.*?\\)")
                       (regexp-quote planner-multi-separator)
                       "+")
               string pos)
              (progn
                (add-to-list 'list (match-string 1 string) t)
                (setq pos (match-end 0)))
            (add-to-list 'list (substring string pos) t)
            (setq pos len)))
        list)
    string))

;;;_+ Tasks

(defadvice planner-task-info-from-string (after planner-multi activate)
  "Store split task strings in position 5 (`planner-task-link')."
  (let ((sub-list (cdr (cdr (cdr (cdr (cdr ad-return-value)))))))
    (when (and (car (cdr sub-list))
               (string-match (regexp-quote planner-multi-separator) (car (cdr sub-list))))
      (setcar sub-list (planner-multi-split (car (cdr sub-list)))))))

(defadvice planner-task-link (around planner-multi activate)
  "Return the next link of a task for compatibility with old code."
  (setq ad-return-value
        (if (listp (nth 5 info))
            (let ((link (planner-multi-next-link (planner-page-name) (nth 5 info))))
              (when link (planner-link-base link)))
          (nth 5 info))))

(defadvice planner-task-plan (around planner-multi activate)
  "Return the first plan of a task for compatibility with old code."
  (if (and (nth 5 info) (listp (nth 5 info)))
      (let ((current (nth 5 info)))
        (while current
          (unless (string-match planner-date-regexp (planner-link-base (car current)))
            (setq ad-return-value (planner-link-base (car current)))
            (setq current nil))
          (setq current (cdr current))))
    ad-do-it))

(defadvice planner-task-date (around planner-multi activate)
  "Return the date assigned to this task."
  (if (and (nth 5 info) (listp (nth 5 info)))
      (let ((current (nth 5 info)))
        (while current
          (when (string-match planner-date-regexp (planner-link-base (car current)))
            (setq ad-return-value (planner-link-base (car current)))
            (setq current nil))
          (setq current (cdr current))))
    ad-do-it))

(defun planner-multi-task-link-as-list (info)
  "Return the page links of INFO as a list."
  (if (listp (nth 5 info))
      (nth 5 info)
    (list (nth 5 info))))

(defun planner-multi-task-xref (page)
  "Copy the current task to PAGE."
  (interactive (list (planner-make-link (planner-read-name (planner-file-alist)))))
  (save-window-excursion
    (goto-char (planner-line-beginning-position))
    (let ((case-fold-search nil))
      ;; Already a multilink
      (cond
       ((re-search-forward (concat
                            "("
                            "\\(" emacs-wiki-name-regexp "\\)"
                            "\\("
                            (regexp-quote planner-multi-separator)
                            "\\("
                            emacs-wiki-name-regexp
                            "\\)\\)+)") (planner-line-end-position) t)
        (forward-char -1)
        (insert planner-multi-separator page))
       ;; Single link
       ((re-search-forward (concat "(\\("
                                   emacs-wiki-name-regexp
                                   "\\))")
                           (planner-line-end-position) t)
        (forward-char -1)
        (insert planner-multi-separator
                page
                planner-multi-separator
                (planner-make-link (planner-page-name))))
       ;; No link yet
       (t
        (re-search-forward "\\s-*$" (planner-line-end-position) t)
        (replace-match (concat " (" page ")") t t)))
      (planner-update-task))))

(defun planner-multi-copy-or-move-task (&optional date force)
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
  (let ((info (planner-current-task-info))
        (case-fold-search nil))
    (if (or (null date)
            (string-match planner-date-regexp date))
        (if (and (planner-task-link-text info)
		 (or (listp (planner-task-link-text info))
		     (string-match
		      (concat "^\\(" emacs-wiki-name-regexp "\\)"
			      "\\(" (regexp-quote planner-multi-separator)
                              "\\(" emacs-wiki-name-regexp
			      "\\)\\)+$")
		      (planner-task-link-text info))))
            (progn
              (when (equal date (planner-page-name))
                (error "Cannot move a task back to the same day"))
              (unless force
                (when (equal (planner-task-date info) date)
                  (error "Cannot move a task back to the same day"))
                (when (equal (planner-task-status info) "X")
                  (error "Cannot reschedule a completed task"))
                (when (equal (planner-task-status info) "C")
                  (error "Cannot reschedule a cancelled task")))
              ;; Multiple pages
              (with-planner-update-setup
               ;; Delete from date page
               (let ((old-date (planner-task-date info))
                     (links (planner-multi-split
                             (planner-task-link-text info))))
                 (when old-date
                   (planner-find-file (planner-link-base old-date))
                   (planner-find-task info)
                   (delete-region (planner-line-beginning-position)
                                  (min (point-max)
                                       (1+ (planner-line-end-position)))))
                 ;; Update
                 (setq links (planner-multi-link-delete
                              (planner-task-date info) links))
                 (planner-find-file (planner-link-base (car links)))
                 (when date (setq links (cons date links)))
                 (if (planner-find-task info)
                     (delete-region (planner-line-beginning-position)
                                    (min (point-max)
                                         (1+ (planner-line-end-position))))
                   (planner-seek-task-creation-point))
                 (insert (planner-format-task info nil nil nil nil
                                              (planner-multi-make-link links))
                         "\n")
                 (forward-char -1)
                 ;; Update all linked tasks
                 (planner-update-task))))
          (planner-copy-or-move-task-basic date force))
      (when (planner-replan-task date) t))))
(defalias 'planner-copy-or-move-task 'planner-multi-copy-or-move-task)

(setq planner-replan-task-function 'planner-multi-replan-task)

(defun planner-multi-replan-task (page-name)
  "Change or assign the plan page for the current task.
PAGE-NAME is the new plan page for the task. Use
`planner-copy-or-move-task' if you want to change the date.
With a prefix, provide the current link text for editing."
  (let ((info (planner-current-task-info))
	old-pages ; Pages the task is currently assigned to
	new-pages
	new-task
	date) ; Pages from page-name
    (when (and (listp page-name)
               (= (length page-name) 1))
      (setq page-name (car page-name)))
    (if (and
	 ;; The proposed link is not a multilink
	 (or (not page-name)
             (and (stringp page-name)
                  (not (string-match planner-multi-separator page-name))))
	 ;; The current link is not a multilink
	 (not (and (planner-task-link-text info)
		   (string-match planner-multi-separator
				 (planner-task-link-text info)))))
	(planner-replan-task-basic page-name)
      (with-planner-update-setup
       ;;; Do the funky multi magic here
       (setq old-pages (planner-multi-task-link-as-list info))
       ;; Adjust the past page name if we're coming from a task with a date
       (setq date (planner-task-date info))
       (when (and date
		  (not (planner-multi-link-member date old-pages)))
	 (setq old-pages (cons date old-pages)))
       ;; Set up the new links list for easy testing
       (setq new-pages (mapcar 'planner-link-base
                               (if (listp page-name)
                                   page-name
                                 (planner-multi-split page-name))))
        ;; Adjust the future page name if we're coming from a task with a date
       (unless (or (null date)
                   (member date new-pages))
	 (setq page-name
	       (concat date planner-multi-separator page-name))
	 (setq new-pages (cons date new-pages)))
       ;; Remove date if it's just that
       (when (and date (equal new-pages (list date)))
	 (setq page-name nil)
	 (setq new-pages nil))
       (setq new-task (planner-format-task info nil nil nil nil
					   (or (planner-make-link page-name) "")
					   (or page-name "")))
       ;; Map over the old pages, removing if not in the new one and updating if it is
       ;; We use mapcar instead of mapc for emacs20 compatibility
       (mapcar (lambda (page)
                 (planner-find-file page)
                 (cond
                  ((planner-find-task info)
                   (delete-region (planner-line-beginning-position)
                                  (1+ (planner-line-end-position))))
                  ((member (planner-page-name) new-pages)
                   (planner-seek-task-creation-point)))
                 (when (or (string-match planner-date-regexp (planner-page-name))
                           (member (planner-page-name) new-pages))
                   (insert new-task "\n"))
                 (setq new-pages (delete (planner-page-name) new-pages)))
               old-pages)
       ;; Map over any new pages that were not included
       (mapcar (lambda (page)
                 (planner-find-file page)
                 (if (planner-find-task info)
                     (delete-region (planner-line-beginning-position)
                                    (1+ (planner-line-end-position)))
                   (planner-seek-task-creation-point))
                 (insert new-task "\n"))
               new-pages)))))

(defun planner-multi-update-task ()
  "Update the current task's priority and status on the linked page.
Tasks are considered the same if they have the same description.
This function allows you to force a task to be recreated if it
disappeared from the associated page.

Note that the text of the task must not change.  If you want to be able
to update the task description, see planner-id.el."
  (interactive)
  (let* ((info (planner-current-task-info))
         (links (planner-multi-task-link-as-list info)))
    (if (<= (length links) 1)
        (planner-update-task-basic)
      ;; Jump around
      (with-planner-update-setup
       (while links
         (planner-find-file (planner-link-base (car links)))
         (if (planner-find-task info)
             ;; Already there, so update only if changed
             (unless (planner-tasks-equal-p info
                                            (planner-current-task-info))
               (delete-region (planner-line-beginning-position)
                              (planner-line-end-position))
               (insert (planner-format-task info)))
           ;; Not yet there, so add it
           (planner-seek-task-creation-point)
           (insert (planner-format-task info) "\n"))
         (setq links (cdr links)))))))
(defalias 'planner-update-task 'planner-multi-update-task)

(defun planner-multi-tasks-equal-p (task-a task-b)
  "Return t if TASK-A and TASK-B are equivalent.
This is true if they have the same value for priority, status,
description, and links."
  (and (string= (or (planner-task-priority task-a) "")
                (or (planner-task-priority task-b) ""))
       (string= (or (planner-task-status task-a) "")
                (or (planner-task-status task-b) ""))
       (string= (or (planner-task-description task-a) "")
                (or (planner-task-description task-b) ""))
       (if (or (string-match (regexp-quote planner-multi-separator)
                             (or (planner-task-link-text task-a) ""))
               (string-match (regexp-quote planner-multi-separator)
                             (or (planner-task-link-text task-b) "")))
           (string= (planner-task-link-text task-a)
                    (planner-task-link-text task-b))
         (and (string= (or (planner-task-plan task-a) "")
                       (or (planner-task-plan task-b) ""))
              (string= (or (planner-task-date task-a) "")
                       (or (planner-task-date task-b) ""))))))
(defalias 'planner-tasks-equal-p 'planner-multi-tasks-equal-p)

(defadvice planner-delete-task (around planner-multi activate)
  "Remove this note from all linked pages."
  ;; Delete the current note.
  (save-window-excursion
    (let ((info (planner-current-task-info))
          links)
      (setq links (planner-multi-task-link-as-list info))
      (if (<= (length links) 1)
          ad-do-it
        (while links
          (planner-find-file (planner-link-base (car links)))
          (when (planner-find-task info)
            (delete-region (planner-line-beginning-position)
                           (min (point-max) (1+ (planner-line-end-position)))))
          (setq links (cdr links)))))))

(defadvice planner-make-link (around planner-multi activate)
  "Escape separately if using `planner-multi-separator'."
  (if (ad-get-arg 2)
      ad-do-it
    (let ((case-fold-search nil))
      (cond
       ((listp link)
        (setq ad-return-value (mapconcat 'emacs-wiki-make-link
                                         link
                                         planner-multi-separator)))
       ((string-match (regexp-quote planner-multi-separator) link)
        (setq ad-return-value (mapconcat 'emacs-wiki-make-link
                                         (planner-multi-split link)
                                         planner-multi-separator)))
       (t ad-do-it)))))

(defun planner-multi-filter-links (regexp links-list &optional nonmatch)
  "Return a list of links matching REGEXP in LINKS-LIST.
If NONMATCH is non-nil, return non-matching links instead."
  (delq nil
        (mapcar (lambda (item)
                  (when (funcall (if nonmatch 'not 'null)
                                 (string-match regexp item))
                    item))
                links-list)))

(defun planner-multi-create-task-from-info (info &optional priority number status description link-text date plan)
  "Create a task in the date and plan pages based on INFO.
Optional arguments PRIORITY, NUMBER, STATUS, DESCRIPTION,
LINK-TEXT, DATE, and PLAN override those in INFO.
Create task on multiple pages if necessary."
  (setq link-text (or link-text (planner-task-link-text info)))
  (when planner-multi-copy-tasks-to-page
    (if link-text
        (unless (string-match (regexp-quote planner-multi-copy-tasks-to-page) link-text)
          (setq link-text (concat link-text planner-multi-separator planner-multi-copy-tasks-to-page)))
      (setq link-text planner-multi-copy-tasks-to-page)))
  (if (and link-text
           (string-match (regexp-quote planner-multi-separator) link-text))
      (progn
        ;; Create the task on all pages.
        (let ((list (planner-multi-split (or link-text (planner-task-link-text info))))
              (link-text (or link-text (planner-task-link-text info))))
          (when (or date (planner-task-date info))
            (setq list (planner-multi-filter-links
                        planner-date-regexp
                        list
                        t))
            (add-to-list 'list (or date (planner-task-date info)))
            (setq link-text (planner-multi-make-link list)))
          (save-window-excursion
            (while list
              (planner-find-file (planner-link-base (car list)))
              (planner-seek-task-creation-point)
              (insert (planner-format-task info priority number status description link-text)
                      "\n")
              (setq list (cdr list)))
            (forward-line -1)
            (run-hooks 'planner-create-task-hook))))
    (planner-create-task-from-info-basic info priority number status
                                         description link-text date plan)))
(setq planner-create-task-from-info-function 'planner-multi-create-task-from-info)

(defun planner-multi-task-delete-this-page ()
  "Remove this task from the current page."
  (interactive)
  ;; Delete the current note.
  (save-window-excursion
    (let ((info (planner-current-task-info))
          new-link-list
          new-link)
      (when info
        (delete-region (planner-line-beginning-position)
                       (min (point-max) (1+ (planner-line-end-position))))
        (setq new-link-list
              (delete (planner-page-name) (copy-sequence (planner-multi-task-link-as-list info))))
        (setq new-link (planner-make-link new-link-list))
        (while new-link-list
          (planner-find-file (planner-link-base (car new-link-list)))
          (planner-find-task info)
          (when (re-search-forward (concat
                                    "("
                                    "\\(" emacs-wiki-name-regexp "\\)"
                                    "\\("
                                    (regexp-quote planner-multi-separator)
                                    "\\("
                                    emacs-wiki-name-regexp
                                    "\\)\\)+)")
                                   (planner-line-end-position)
                                   t)
            (replace-match
             (if (string= new-link "") ""
               (concat "(" new-link ")"))
             t t))
          (setq new-link-list (cdr new-link-list)))))))

(defun planner-multi-edit-task-description (description)
  "Update multiple pages."
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
           new-task)
       (setq new-task (planner-format-task info nil nil nil description))
       (let ((links (planner-multi-task-link-as-list info)))
         (if (<= (length links) 1)
             (planner-edit-task-description-basic description)
           ;; Jump around
           (while links
             (planner-find-file (planner-link-base (car links)))
             (if (planner-find-task info)
                 ;; Already there, so update only if changed
                 (delete-region (planner-line-beginning-position)
                                (min (point-max)
                                     (1+ (planner-line-end-position))))
               ;; Not yet there, so add it
               (planner-seek-task-creation-point))
             (insert new-task "\n")
             (setq links (cdr links)))))))
    (goto-char point)))
(defalias 'planner-edit-task-description 'planner-multi-edit-task-description)

;; Todo: copy-or-move

;;;_+ Notes

(defadvice planner-current-note-info (after planner-multi activate)
  "Store split note strings in position 4 (`planner-note-link')."
  (let ((sub-list (cdr (cdr (cdr (cdr ad-return-value))))))
    (when (and (car sub-list) (string-match (regexp-quote planner-multi-separator) (car sub-list)))
      (setcar sub-list (planner-multi-split (car sub-list))))))

(defadvice planner-note-link (around planner-multi activate)
  "Return the next link of a note for compatibility with old code."
  (setq ad-return-value
        (if (and (nth 4 note-info) (listp (nth 4 (ad-get-arg 0))))
            (planner-multi-next-link (planner-page-name) (nth 4 (ad-get-arg 0)))
          (nth 4 note-info))))

(defadvice planner-note-link-text (around planner-multi activate)
  "Return the link text of a note for compatibility with old code."
  (setq ad-return-value
        (if (and (nth 4 note-info) (listp (nth 4 (ad-get-arg 0))))
            (planner-make-link (nth 4 note-info))
          (nth 4 note-info))))

(defun planner-multi-note-link-as-list (info)
  "Return the page links of INFO as a list."
  (if (listp (nth 4 info))
      (nth 4 info)
    (list (nth 4 info))))

(defadvice planner-update-note (around planner-multi activate)
  "Copy the text from this note to the linked notes, if any."
  (interactive)
  (let ((info (planner-current-note-info))
        body)
    (if (= (length (planner-multi-note-link-as-list info)) 1)
        ad-do-it
      (save-window-excursion
        (save-restriction
          (planner-narrow-to-note)
          (goto-char (point-min))
          (skip-chars-forward ".#0-9")
          (setq body (buffer-substring-no-properties (point) (point-max))))
        (let ((links (planner-multi-note-link-as-list info)))
          ;; Jump around
          (while links
            (planner-visit-link (planner-link-target (car links)))
            (save-restriction
              (when (planner-narrow-to-note)
                (goto-char (point-min))
                (skip-chars-forward ".#0-9")
                (delete-region (point) (point-max))
                (insert body)))
            (setq links (cdr links))))))))

(defun planner-multi-note-xref (page)
  "Copy the current note to PAGE."
  (interactive (list (planner-read-name (planner-file-alist))))
  (let ((case-fold-search nil))
    ;; Modify the current note
    (save-window-excursion
      (when (planner-current-note-info)
        (let (note-num)
          (save-window-excursion
            (setq note-num (planner-create-note page)))
          ;; Add to current
          (unless (looking-at "^.#[0-9]")
            (re-search-backward "^.#[0-9]" nil t))
          (cond
           ;; Already a multilink
           ((re-search-forward (concat
                                "("
                                "\\(" emacs-wiki-name-regexp "\\)"
                                "\\("
                                (regexp-quote planner-multi-separator)
                                "\\("
                                emacs-wiki-name-regexp
                                "\\)\\)+)") (planner-line-end-position) t)
            (forward-char -1)
            (insert planner-multi-separator
                    (planner-make-link (concat page "#"
                                               (number-to-string note-num)))))
           ;; Single link
           ((re-search-forward (concat "(\\("
                                       emacs-wiki-name-regexp
                                       "\\))")
                               (planner-line-end-position) t)
            (forward-char -1)
            (insert planner-multi-separator
                    (planner-make-link (concat page "#"
                                               (number-to-string note-num)))
                    planner-multi-separator
                    (planner-make-link (concat (planner-page-name) "#"
                                               (planner-note-anchor (planner-current-note-info))))))
           ;; No link yet
           (t
            (re-search-forward "\\s-*$" (planner-line-end-position) t)
            (replace-match
             (concat " ("
                     (save-match-data
                       (planner-make-link (concat page "#"
                                                  (number-to-string note-num))))
                     ")")
             t t)))
          (planner-update-note))))))

(defadvice planner-replan-note (around planner-multi activate)
  "Allow multiple pages."
  (if (string-match (regexp-quote planner-multi-separator) page)
      (save-window-excursion
        (save-restriction
          (let* ((info (planner-current-note-info))
                 (links (planner-multi-note-link-as-list info))
                 (new-links (planner-multi-split page))
		 (old-anchor (planner-make-link
			      (concat (planner-note-page info) "#"
				      (planner-note-anchor info))))
		 cursor
                 current-page
                 (old-pages (mapcar 'planner-link-base links)))
	    (when (and (not old-pages)
		       (string-match planner-date-regexp (planner-note-page info)))
	      (setq old-pages (list (planner-note-page info)))
              (add-to-list 'new-links old-anchor))
            ;; Add to new pages
	    (setq cursor new-links)
            (while cursor
              (setq current-page (planner-link-base (car cursor)))
              (when (not (member current-page old-pages))
                (setcar cursor
                        (planner-make-link
                         (format "%s#%d"
                                 (planner-link-base (car cursor))
                                 (planner-create-note current-page)))))
              (setq cursor (cdr cursor)))
            ;; Delete from pages removed from list
            (setq cursor links)
            (while cursor
              (unless (member (planner-link-base (car cursor)) old-pages)
                (planner-visit-link (planner-link-target (car links)))
                (save-restriction
                  (when (planner-narrow-to-note)
                    (delete-region (point-min) (point-max)))))
              (setq cursor (cdr cursor)))
            ;; Update the current note
            (planner-visit-link (concat (planner-note-page info)
                                        "#" (planner-note-anchor info)))
            (delete-region (1+ (point)) (planner-line-end-position))
            (insert " " (planner-format-note info "" nil nil (planner-make-link
                                                              new-links)))
            (planner-update-note))))
    ad-do-it))

(defun planner-multi-note-delete ()
  "Remove this note from all linked pages."
  (interactive)
  ;; Delete the current note.
  (save-window-excursion
    (let ((info (planner-current-note-info))
          links)
      (when info
        (setq links (planner-multi-note-link-as-list info))
        (while links
          (planner-visit-link (planner-link-target (car links)))
          (save-restriction
            (planner-narrow-to-note)
            (delete-region (point-min) (point-max)))
          (setq links (cdr links)))))))

(defun planner-multi-note-delete-this-page ()
  "Remove this note from the current page."
  (interactive)
  ;; Delete the current note.
  (save-window-excursion
    (let* ((info (planner-current-note-info))
           new-link
           new-link-list)
      (when info
        (setq new-link-list
              (planner-multi-link-delete
               (planner-page-name)
               (planner-multi-note-link-as-list info)))
        (setq new-link (planner-multi-make-link new-link-list))
        (save-restriction
          (planner-narrow-to-note)
          (delete-region (point-min) (point-max)))
        (while new-link-list
          (planner-visit-link (planner-link-target (car new-link-list)))
          (when (re-search-forward (concat
                                    "("
                                    "\\(" emacs-wiki-name-regexp "\\)"
                                    "\\("
                                    (regexp-quote planner-multi-separator)
                                    "\\("
                                    emacs-wiki-name-regexp
                                    "\\)\\)+)")
                                   (planner-line-end-position)
                                   t)
            (replace-match
             (if (string= new-link "") ""
               (concat "(" new-link ")"))
             t t))
          (setq new-link-list (cdr new-link-list)))))))

(defalias 'planner-multi-xref-note 'planner-multi-note-xref)
(defalias 'planner-multi-delete-note 'planner-multi-note-delete)
(defalias 'planner-multi-delete-note-this-page 'planner-multi-note-delete-this-page)
(defalias 'planner-multi-xref-task 'planner-multi-task-xref)
(defalias 'planner-multi-delete-task-this-page 'planner-multi-task-delete-this-page)

(defun planner-multi-read-name (file-alist &optional prompt initial)
  "Read multiple pages, completing based on FILE-ALIST.
If PROMPT is specified, use that instead of \"Page:\"."
  (let* ((minibuffer-prompt-properties
	  (plist-put
	   (copy-sequence minibuffer-prompt-properties)
	   'read-only nil))
	 (map (copy-keymap minibuffer-local-completion-map))
         (completion-ignore-case t)
         (crm-separator (regexp-quote planner-multi-separator))
         (prompt (format "%s(default: %s) "
			 (or prompt "Page: ") planner-default-page))
         str)
    (unwind-protect
        (progn
          (define-key minibuffer-local-completion-map
	    planner-multi-separator 'self-insert-command)
          (setq str
                (if (fboundp 'completing-read-multiple)
                    (completing-read-multiple
                     prompt file-alist nil nil initial
                     'planner-history-list
                     planner-default-page)
                  (planner-multi-split
                   (read-string prompt initial 'planner-history-list
				planner-default-page))))
          (cond
           ((or (null str)
                (string= (car str) "")) planner-default-page)
           ((string= (car str) "nil") nil)
           (t (mapconcat 'identity str planner-multi-separator))))
      (setq minibuffer-local-completion-map map))))

(defun planner-multi-read-name-multiple-prompts (file-alist prompt initial)
  "Read multiple pages, completing based on FILE-ALIST.
If PROMPT is specified, use that instead of \"Page:\".
Enter one plan page at a time. To end input, type \"nil\"."
  (let ((more t)
        input
        last
        list)
    (while more
      (setq input
	    (planner-read-name-single file-alist
				      (concat "nil to stop. " (or prompt "Page: "))
				      initial))
      (setq planner-default-page nil initial nil)
      (if input
	  (add-to-list 'list input t)
	(setq more nil)))
    (setq planner-default-page (planner-multi-make-link list))))

(if (fboundp 'completing-read-multiple)
    (setq planner-read-name-function 'planner-multi-read-name)
  (setq planner-read-name-function 'planner-multi-read-name-multiple-prompts))

;;;###autoload
(defun planner-multi-remove-task-from-pool (old-status new-status)
  "Remove completed tasks from `planner-multi-copy-tasks-to-page' if that still leaves them linked."
  (when (and planner-multi-copy-tasks-to-page
             (or (string= new-status "C")
                 (string= new-status "X")))
    (let ((info (planner-current-task-info)))
      (when (planner-task-link-text info)
        ;; If it is linked to TaskPool _and_ at least one other thing
        (cond
         ((string-match planner-multi-separator (planner-task-link-text info))
          (let ((remove-from
                 (mapcar 'planner-link-base (planner-multi-split planner-multi-copy-tasks-to-page)))
                new-links)
            (setq new-links
                  (delq nil
                        (mapcar (lambda (item)
                                  (unless (member (planner-link-base item) remove-from)
                                    (planner-link-base item)))
                             (planner-multi-task-link-as-list info))))
            (save-excursion
              (planner-replan-task (mapconcat 'identity new-links planner-multi-separator)))
            ;; Make sure we are on the same task
            (when (string= (planner-page-name) planner-multi-copy-tasks-to-page)
              (planner-find-file (car new-links))
              (planner-find-task info))))
          ;; Else if it has a date and is linked to TaskPool
         ((and (planner-task-date info)
               (string= (planner-task-plan info) planner-multi-copy-tasks-to-page)
          (save-excursion (planner-replan-task nil))
          (when (string= (planner-page-name) planner-multi-copy-tasks-to-page)
            (planner-find-file (planner-task-date info))
            (planner-find-task info))))))))
  t)

(add-hook 'planner-mark-task-hook 'planner-multi-remove-task-from-pool t)

(provide 'planner-multi)

;;; planner-multi.el ends here
