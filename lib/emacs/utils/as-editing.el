;; Adam's editing utilities

;; Should be autoloaded by as-init.el

;;{{{ as-duplicate-line

(defun as-duplicate-line () 
  "Duplicates the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((bol (point))
          line-to-copy)
      (end-of-line)
      (setq line-to-copy (buffer-substring bol (point)))
      (insert "\n" line-to-copy)))
  (next-line 1) ;; sorry FSF
  )

;;}}}
;;{{{ as-join-line-with-next

(defun as-join-line-with-next ()
  "Joins the current line with the next.  This just calls join-line with
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
;;{{{ vim-yy

(defun vim-yy (&optional lines) "doc string"
  (interactive "p")
  (save-excursion
    (setq lines (if (or (null lines)
                        (< lines 1)) 1 lines))
    (beginning-of-line)
    (push-mark (point) nil t)
    (next-line lines)
    (kill-ring-save (mark) (point))))

;;}}}
