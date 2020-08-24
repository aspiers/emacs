;;;###autoload
(defun org-count-keywords ()
  "Count number of occurrences of TODO keywords in the current
buffer, respecting any scope restriction."
  (let (result)
    (org-map-entries
     (lambda ()
       (let ((keyword (elt (org-heading-components) 2)))
         (if keyword
             (if (assoc keyword result)
                 (setcdr (assoc keyword result)
                         (1+ (cdr (assoc keyword result))))
               (setq result (cons (cons keyword 1) result)))))))
    result))

;;;###autoload
(defun org-count-matches (search)
  "Count the number of matches from the given tag search in the
current buffer, respecting any scope restriction."
  (interactive "sMatch: ")
  (length (org-map-entries t search)))

;;;###autoload
(defun org-dblock-write:count (params)
  "Write a table showing the number of occurrences of each of the
specified keywords and tag searches.  Example usage:

#+BEGIN:dynamic block
#+BEGIN: count :keywords (\"NEXT\" \"DONE\") :searches (\"@phone\" \"@home\")
| NEXT       | 522 |
| DONE       |  69 |
| @phone     |  77 |
| @home      | 182 |
#+END:
"
  (let ((keywords (plist-get params :keywords))
        (searches (plist-get params :searches))
        (format "| %-10s | %3d |\n"))
    (insert
     (concat
      (mapconcat (lambda (keyword)
                   (format format keyword (org-count-matches
                                           (concat "/" keyword))))
                 keywords "")
      (mapconcat (lambda (search)
                   (format format search (org-count-matches search)))
                 searches "")))
    (backward-delete-char 1)))

(provide 'as-org-stats)
