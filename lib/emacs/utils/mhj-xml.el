(defun mhj-format-xml ()
  "Put newlines after the beginning and end of every tag, unless 
the tag contained some text, then indent the whole lot so that
it looks nicer."
  (interactive)
  (let ((begin (point))
        (end (point-max)))
    (when mark-active
      (setq begin (region-beginning)
            end (region-end)))
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (search-forward ">" end 'noerror)
        (let ((prev-is-newline (skip-chars-forward " \n")))
          (if (and (looking-at "<") 
                   (= prev-is-newline 0))
              (progn 
                (newline)
                (setq end (1+ end))))))
      (indent-region begin end nil))))
