;;;###autoload
(defun bn-end-of-line-but-one (arg)
  "Move point to one character before the end of current line.
With argument ARG not nil or 1, move forward ARG - 1 lines first.
If scan reaches end of buffer, stop there without error.
If the line is empty, doesn't do anything."
  (interactive "*p")
  (end-of-line arg)
  (unless (bolp)
    (backward-char)))

(provide 'bn-end-of-line-but-one)
