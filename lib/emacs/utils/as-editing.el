;; Adam's editing utilities

;; Should be autoloaded by as-init.el

;;{{{ as-duplicate-line

(defun as-duplicate-line (&optional count) 
  "Duplicates the current line."
  (interactive "p")
  (save-excursion
    (let ((i 0))
      (while (< i count)
        (beginning-of-line)
        (let ((bol (point))
              line-to-copy)
          (end-of-line)
          (setq line-to-copy (buffer-substring bol (point)))
          (insert "\n" line-to-copy))
        (setq i (1+ i)))))
  (next-line count) ;; sorry FSF
  )

;;}}}
;;{{{ as-join-line-with-next

(defun as-join-line-with-next ()
  "Joins the current line with the next.  This just calls `join-line' with
a prefix argument."
  (interactive)
  (join-line 1))

;;}}}
;;{{{ as-copy-previous-line-suffix

(defun as-copy-previous-line-suffix ()
  "Copy the suffix of the line directly above the point and yank it at the point."
  (interactive)
  (save-excursion
    (let ((suffix (save-excursion
                    (previous-line 1) ;; sorry FSF
                    (let ((suffix-start (point)))
                      (end-of-line)
                      (buffer-substring suffix-start (point))))))
      (insert suffix))))

;;}}}
;;{{{ as-align-to-previous-line

(defun as-align-to-previous-line ()
  "Runs indent-relative on the current line and moves down to enable repeating."
  (interactive)
  (save-excursion
    (re-search-forward "[ \t]")
    (re-search-forward "[^ ]")
    (backward-char 1)
    (indent-relative))
  (next-line 1) ;; sorry FSF
  )

;;}}}
;;{{{ as-transpose-lines

(defun as-transpose-lines (arg)
  "Just like `transpose-lines', but preserves the point's position."
  (interactive "*p")
  (let ((col (current-column)))
    (transpose-lines arg)
    (forward-line (- arg))
    (move-to-column col)))

;;}}}
;;{{{ mark-list

(defun mark-list ()
  "Put point at end of this list, mark at beginning.
The list marked is the one that contains point or follows point."
  (interactive)
  (forward-list)
  (push-mark (point) t t)
  (backward-list)
)

;;}}}
;;{{{ vim-yy

(defun vim-yy (&optional lines)
  "Simulates vi's yy command."
  (interactive "p")
  (save-excursion
    (setq lines (if (or (null lines)
                        (< lines 1)) 1 lines))
    (beginning-of-line)
    (push-mark (point) nil t)
    (next-line lines)
    (kill-ring-save (mark) (point))))

;;}}}
;;{{{ Ben's query-replace history advice hack

;; The default behaviour of `query-replace' is slightly
;; annoying.  If we perform a query-replace with the region
;; active, to restrict activity to that region, then want to
;; perform the same query-replace on another region, we can't
;; use `repeat-complex-command' because the evaluated values of
;; `region-beginning' and `region-end' get stored in the command
;; history.  We fix that by defining some advice for the common
;; worker function for all replacement-type functions,
;; `perform-replace'.

(defvar bn-replace-functions-to-fix-up
  (list 'query-replace
        'query-replace-regexp
        'query-replace-regexp-eval
        'map-query-replace-regexp
        'replace-string
        'replace-regexp)
  "List of functions for which the `bn-fix-command-history' advice
will doctor `command-history'.")

(defun bn-safe-region-beginning ()
  "Return the beginning of region, or nil if region not active"
  (if mark-active
      (region-beginning)
    nil))

(defun bn-safe-region-end ()
  "Return the end of region, or nil if region not active"
  (if mark-active
      (region-end)
    nil))

(defadvice perform-replace (after bn-fix-command-history)
  "Fix up the most recent entry in `command-history' so that the
START and END arguments are replaced by calls to
`bn-safe-region-beginning' and `bn-safe-region-end'
respectively.  This allows `repeat-complex-command' to do the
Right Thing with `query-replace' and friends."
  (let ((ch-top (car command-history)))
    ;; This check may be unnecessary, but just in case we get
    ;; called as part of another interactive function, make sure
    ;; we're only changing history for suitable functions.
    (when (memq (car-safe ch-top) bn-replace-functions-to-fix-up)
      (setcar (cddddr ch-top) (list 'bn-safe-region-beginning))
      (setcar (cdr (cddddr ch-top)) (list 'bn-safe-region-end)))))

(ad-activate 'perform-replace)

;;}}}
