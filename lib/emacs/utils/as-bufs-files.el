;; Adam's buffer/file utilities

;; Should be autoloaded by as-init.el

;;{{{ as-bounce-buffer

(defvar as-bounce-buffer-regexp-alist '()
  "Controls the behaviour of `as-bounce-buffer'.")

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

;;}}}
;;{{{ as-display-buffer-filename

(defun as-display-buffer-filename
  ()
  "Displays the current buffer's filename in the minibuffer."
  (interactive)
  (message (or buffer-file-name "No file associated with this buffer")))

;;}}}
;;{{{ as-destroy-buffer

(defun as-destroy-buffer ()
  "Kill the current buffer without leaving crappy auto-save files around."
  (interactive)
  (let ((tmpfile (format "/tmp/.emacs.as-destroy-buffer.%d" (emacs-pid)))
        (buf (buffer-name)))
    (write-file tmpfile)
    (kill-buffer nil)
    (delete-file tmpfile)
    (message (concat "Destroyed buffer " buf))))

;;}}}
;;{{{ bury-and-close-buffer

(defun bury-and-close-buffer ()
  (interactive)
  (bury-buffer) 
  (when (> (length (window-list)) 1) 
    (delete-window)))

;;}}}
;;{{{ mhj-set-q-to-close

(defun mhj-set-q-to-close ()
  (local-set-key "q" 'bury-and-close-buffer))

;;}}}

