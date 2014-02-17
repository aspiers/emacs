;;;###autoload
(defun as-duplicate-line (&optional count) 
  "Duplicates the current line."
  (interactive "*p")
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
  (let ((line-move-visual nil))
    ;; Deliberately not using forward-line in order to preserve
    ;; column of cursor
    (next-line count)))

;;;###autoload
(defun as-join-line-with-next (&optional preserve-comment)
  "Joins the current line with the next.  Removes any
continuation backslash from the end of the line, and any prefix
matching `adaptive-fill-regexp' from the beginning of the
next (unless a prefix argument is given) before joining."
  (interactive "*P")

  (save-excursion
    (end-of-line)
    (forward-char -1)
    (and (looking-at "\\\\") (delete-char 1))
    (unless (or preserve-comment (not comment-start))
        (save-excursion
          (forward-line 1)
          (let ((beg (point)))
            (if (looking-at adaptive-fill-regexp)
                (replace-match "")))))
    (join-line 1)))

;;;###autoload
(defun as-copy-previous-line-suffix ()
  "Copy the suffix of the line directly above the point and yank it at the point."
  (interactive "*")
  (save-excursion
    (let ((suffix (save-excursion
                    (let ((col (current-column)))
                      (forward-line -1)
                      (move-to-column col))
                    (let ((suffix-start (point)))
                      (end-of-line)
                      (buffer-substring suffix-start (point))))))
      (insert suffix))))

;;;###autoload
(defun as-align-to-previous-line (&optional count)
  "Runs indent-relative on the current line and moves down to enable repeating."
  (interactive "*p")
  (let ((i 0))
    (while (< i count)
      (save-excursion
        (backward-char 1)
        (re-search-forward "[ \t]")
        (re-search-forward "[^ \t]")
        (backward-char 1)
        (indent-relative))
      (next-line 1) ;; sorry FSF
      (setq i (1+ i)))))

;;;###autoload
(defun as-transpose-lines (arg)
  "Just like `transpose-lines', but preserves the point's position."
  (interactive "*p")
  (let ((col (current-column)))
    (transpose-lines arg)
    (forward-line (- arg))
    (move-to-column col)))

;;;###autoload
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

(provide 'as-line-ops)
