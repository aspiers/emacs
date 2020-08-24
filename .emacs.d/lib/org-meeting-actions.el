;;;###autoload
(defun org-dblock-write:extract-actions (params)
  "Dynamic block writer which extracts headlines from a file and
generates a table with one row per headline.

The `:keyword' parameter determines the tag search to use for
selecting which headlines to extract."
  ;; TODO: pass :delimiter param to table-row-todo-owner
  (insert
   (concat
    "| Owner | Action |
| / | < |
|-
"
    (apply 'concat
           ;; need a (car ) around this next bit if I've patched
           ;; org-map-entries to return a list per file
            (org-map-entries 'org-dblock-write:table-row-todo-owner
                             (or (plist-get params :keyword) "/ACTION")
                             'file)))
;;    (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
;;      (insert "Last block update at: "
;;              (format-time-string fmt (current-time)))))
   )
  (backward-delete-char 1) ;; not sure where the extra \n comes from
  (org-table-align))

(defun org-dblock-write:table-row-todo-owner (&optional owner-delim)
  "Generates a row in an actions table from a single headline at
point."
  (let ((owners (mapconcat 'identity
                           (org-get-tags)
                           (or owner-delim ", "))))
    (if (looking-at org-complex-heading-regexp)
        (let ((item (match-string 4)))
          (concat "| " owners " | " item " |\n"))
      (error "Odd, '%s' didn't match '%s'"
             (buffer-substring (point) (+ (point) 20))
             org-complex-heading-regexp))))

(provide 'org-meeting-actions)
