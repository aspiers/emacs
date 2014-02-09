;; Things to help me with Getting Things Done.

(require 'as-mairix)
(require 'org)

;;;###autoload
(defun as-org-convert-buffer-sub-to-effort ()
  "Convert all 'sub*' tags within a buffer into 'Effort' properties."
  (interactive)
  (org-map-entries 'as-org-convert-headline-sub-to-effort nil 'file))  

(defun as-org-convert-headline-sub-to-effort ()
  "Convert a headline with a 'sub*' tag into an 'Effort' property."
  (interactive)
  (unless (org-on-heading-p)
    (error "Not on heading"))
  (let ((origtags (org-get-tags)))
    (mapcar
     (lambda (tag)
       (when (equal (substring tag 0 3) "sub")
         (org-set-property "Effort"
                           (cdr (assoc (substring tag 3)
                                       '(("10"  . "0:10")
                                         ("30"  . "0:30")
                                         ("60"  . "1:00")
                                         ("120" . "2:00")
                                         ("4"   . "4:00")
                                         ("day" . "8:00")))))
         (org-toggle-tag tag 'off)))
     origtags)))

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

;;;###autoload
(defun as-org-switch-to-agenda-buffer ()
  "Switch to an existing *Org Agenda* buffer, otherwise run
`org-agenda' with Adam's custom day view."
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (org-agenda nil "d")))

(defun as-org-clock-in-switch-to-state (current-state)
  "Function for use with `org-clock-in-switch-to-state'."
  (if (equal current-state "NEXT") "STARTED" current-state))

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

(defun as-org-jump-clock-or-agenda ()
  "Jump to an active clock or to the agenda buffer if no clock is active."
  (interactive)
  (if (marker-buffer org-clock-marker)
      (org-clock-goto)
    (as-org-switch-to-agenda-buffer)))

(bind-key "i" 'org-agenda-clock-in  org-agenda-mode-map)
(bind-key "o" 'org-agenda-clock-out org-agenda-mode-map)

(provide 'as-gtd)
