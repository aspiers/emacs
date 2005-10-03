;;; planner-experimental.el --- Experimental functions for Emacs planner mode
;;; Experimental functions for planner.el

;;; Commentary:
;;
;; Version: 2005.08.20-17.59-stable

(require 'planner)

;;; Code:

;;;_ + Searching

(defun planner-search-notes-next-match ()
  "Jump to the next matching entry.  Call after `planner-search-notes.'"
  (interactive)
  (if (buffer-live-p (get-buffer planner-search-notes-buffer))
      (progn
        (set-buffer planner-search-notes-buffer)
        (forward-line -1)
        (goto-char (planner-line-beginning-position))
        (planner-follow-name-at-point))
    (error "No current search.")))

(defun planner-search-notes-previous-match ()
  "Jump to the previous matching entry.  Call after `planner-search-notes.'"
  (interactive)
  (if (buffer-live-p (get-buffer planner-search-notes-buffer))
      (progn
        (set-buffer planner-search-notes-buffer)
        (forward-line 1)
        (goto-char (planner-line-beginning-position))
        (planner-follow-name-at-point))
    (error "No current search.")))

;;;_* Tasks

(defun planner-remove-duplicates ()
  "Remove duplicate tasks."
  (interactive)
  (goto-char (point-min))
  (let ((today (planner-today))
        (on-date (string-match planner-date-regexp (planner-page-name))))
    (while (re-search-forward "^#[A-C][0-9]*\\s-+\\(.+\\)$" nil t)
      (save-excursion
        (let* ((task-info (planner-current-task-info))
               (keep (planner-task-date task-info))
               date
               found
               (start (match-beginning 0)))
          (goto-char (planner-line-beginning-position))
          (save-excursion
            (unless on-date
              (while (planner-find-task task-info (point))
                ;; Determine the most recent one
                (setq date (planner-task-date (planner-current-task-info)))
                (when (or (and (string< keep today)
                               (string< keep date))
                          (string< date keep))
                (setq keep date))
                (forward-line 1))))
          (while (planner-find-task task-info (point))
            (if (string= keep
                         (planner-task-date (planner-current-task-info)))
                (if found
                    (delete-region (planner-line-beginning-position)
                                   (min (1+ (planner-line-end-position))
                                        (point-max)))
                  (setq found t)
                  (forward-line 1))
              (planner-delete-task))))))))

;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (-1 0 : )
;;;End:

(provide 'planner-experimental)

;;; planner-experimental.el ends here
