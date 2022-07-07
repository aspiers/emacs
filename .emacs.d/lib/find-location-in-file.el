(defun parse-file-and-location (str)
  "Parse a string to extract a file path and (optionally, if they are
present) line and column numbers."
  (cond
   ((string-match "^\\(\\.?/.+\\) \\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$" str)
    (list (match-string 1 str)
          (string-to-number (match-string 2 str))
          (string-to-number (match-string 3 str))))
   ((string-match "^\\(\\.?/.+\\)(\\([0-9]+\\),\\([0-9]+\\))$" str)
    (list (match-string 1 str)
          (string-to-number (match-string 2 str))
          (string-to-number (match-string 3 str))))
   ((string-match "^\\(\\.?/.+\\)\\+\\([0-9]+\\):\\([0-9]+\\)$" str)
    (list (match-string 1 str)
          (string-to-number (match-string 2 str))
          (string-to-number (match-string 3 str))))
   (t str)))

(defun find-location-in-file (file &optional line column)
  "Find a file, optionally specifying a line and even column within
that file to jump to.

LINE starts at 1 but COLUMN starts at 0."
  (find-file file)
  (when line (goto-line line))
  (when column (forward-char column)))

(defun find-file-with-location (str)
  "Find a file with an optional line and column location to jump to."
  (apply 'find-location-in-file (parse-file-and-location str)))

(provide 'find-location-in-file)
