;;;###autoload
(defun as-org-switch-to-agenda-buffer (&optional mode redisplay)
  "Switch to an existing *Org Agenda(%s)* buffer, otherwise run
`org-agenda' to generate that buffer and then switch to it."
  (interactive)
  (setq mode (or mode "a"))
  (let ((buf (format "*Org Agenda(%s)*" mode)))
    (cond ((get-buffer buf)
           (switch-to-buffer buf))
          (t
           (message "Loading %s..." buf)
           (org-agenda nil mode)
           (message "Loading %s...done" buf)))
    (when redisplay
      (message "Refreshing %s..." buf)
      (redisplay)
      (org-agenda-redo-all)
      (message "Refreshing %s...done" buf))))

;;;###autoload
(defun as-org-agenda-skip-select-category-function (category-to-select)
  "Creates a function suitable for use with
`org-agenda-skip-function' which skips all items except for those
in the provided category.

From the docs for `org-agenda-skip-function', if the returned
function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the
search should be continued."
  `(lambda ()
     (let ((cat (org-get-category)))
       ;;(message "Is '%s' '%s' ?" cat ,category-to-select)
       (if (equal cat ,category-to-select)
           nil ;; don't skip
         ;; the invisible-ok param below is crucial to avoid infinite loops
         (org-end-of-subtree t)))))

(provide 'as-org-agenda)
