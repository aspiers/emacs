;; fill-common-prefix-region

(autoload 'cl-flet "cl"
  nil ;; not interactive
  'macro)

;;;###autoload
(defun fill-common-prefix-region (&optional justify nosqueeze)
  "Call `fill-region' on the largest region surrounding the
current point within which all line beginnings yield exactly the
same when matched against `adaptive-fill-regexp'.

A line containing nothing but the common prefix and possibly trailing
whitespace is treated as a region boundary.

This is particularly useful for filling subsections of
paragraphs, e.g. email with different levels of nested \"> \"
quoting, where by default emacs considers all the different
levels as part of one big paragraph."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full))))
  (save-excursion
    (beginning-of-line)
    (or (looking-at adaptive-fill-regexp)
        (error "Current line prefix does not match adaptive-fill-regexp"))
    (let* ((common-prefix (match-string 0))
           (common-prefix-nw (concat (regexp-quote common-prefix)
                                     "[^\n]*[^[:space:]\n]")))
      (cl-flet
       ((find-limit (result-fn buffer-limit-fn line-step)
                    (save-excursion
                      (let (result)
                        ;; Keep looking while we see the same fill-prefix
                        ;; followed by some non-whitespace.
                        (while (and (setq result (funcall result-fn))
                                    (not (funcall buffer-limit-fn))
                                    (forward-line line-step)
                                    (looking-at adaptive-fill-regexp)
                                    (equal common-prefix (match-string 0))
                                    (looking-at common-prefix-nw)))
                        result))))
       (let ((start (find-limit #'point #'bobp -1))
             (end (find-limit #'line-end-position #'eobp 1)))
         (fill-region start end justify nosqueeze))))))

(provide 'fill-common-prefix-region)
