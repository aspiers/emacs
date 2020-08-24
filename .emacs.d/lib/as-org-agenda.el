;;;###autoload
(defun as-org-switch-to-agenda-buffer ()
  "Switch to an existing *Org Agenda* buffer, otherwise run
`org-agenda' with Adam's custom day view."
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (org-agenda nil "d")))

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
