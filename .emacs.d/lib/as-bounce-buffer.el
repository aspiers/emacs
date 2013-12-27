(defvar as-bounce-buffer-regexp-alist '()
  "Controls the behaviour of `as-bounce-buffer'.")

;;;###autoload
(defun as-bounce-buffer ()
  "For each element of `as-bounce-buffer-regexp-alist', attempts a search
and replace on the current buffer's filename.  (The CARs are the
search regexps, and the CDRs the corresponding strings to replace the
matches with).  As soon a search is successful, the filename resulting
from the replace is visited via `find-file'."
  (interactive)
  (catch 'gotcha
    (mapcar
     (lambda (x)
       (let ((case-fold-search nil) 
             (match (car x))
             (replace (cdr x)))
         (cond
          ((string-match match (buffer-file-name))
           (let ((bounce-to (replace-match replace t t (buffer-file-name) nil)))
             (message (format "Bounced to %s" bounce-to))
             (find-file bounce-to))
           (throw 'gotcha nil)))))
     as-bounce-buffer-regexp-alist)))

(provide 'as-bounce-buffer)
