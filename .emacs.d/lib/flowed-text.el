(defun format-as-flowed-text ()
  "Format the buffer as flowed text according to RFC 2646.
This ensures that appropriate lines should be terminated with a
single space, and that \"> \" quoting prefixes are replaced with
\">\".  Operates on the current region if active, otherwise on
the whole buffer."
  (interactive)
  (let ((start (if (use-region-p) (copy-marker (region-beginning)) (point-min-marker)))
        (end (if (use-region-p) (copy-marker (region-end)) (point-max-marker))))
    (save-excursion
      (goto-char start)
      ;; Ensure appropriate lines end with a space
      (while (re-search-forward "^\\(>+ ?\\)?\\S-.\\{10,\\}\\S-$"
                                (marker-position end) t)
        (replace-match "\\& " t))

      ;; Replace "> " quoting prefixes with ">"
      (goto-char (marker-position start))
      (let ((eol)
            (eolm (make-marker)))
        (while (setq eol (re-search-forward "^>.*" end t))
          (set-marker eolm eol)
          (goto-char (match-beginning 0))
          (while (looking-at ">")
            (if (looking-at "> \\([^  ]\\)")
                (replace-match ">\\1")
              (forward-char)))
          (goto-char (marker-position eolm)))))))

(provide 'flowed-text)
