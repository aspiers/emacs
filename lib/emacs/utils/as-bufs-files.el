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

(eval-when-compile
  (autoload 'x-select-text "term/x-win" nil t))
(defun as-display-buffer-filename (&optional save-to-clipboard)
  "Displays the current buffer's filename in the minibuffer.

If a prefix argument is given, stores the result in the kill ring
and in the X selection for other programs.  If the prefix argument
is 2, forks the external program abs (must be in $PATH) to convert
the filename to an absolute one with all symlinks resolved."
  (interactive "P")
  (or buffer-file-name (error "No file associated with this buffer"))
  (let ((fn (cond ((and save-to-clipboard (eq save-to-clipboard 2))
                   (substring
                    (shell-command-to-string (concat "abs " buffer-file-name))
                    0 -1))
                  (t buffer-file-name))))
    (cond (save-to-clipboard
           (kill-new fn)
;; No need for x-select-text since kill-new calls function set
;; in interprogram-cut-function, which is x-select-text anyway.
;;         (x-select-text buffer-file-name)
           (setq fn (concat fn " (stored in kill ring and X selection)"))))
    (message fn)))

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
;;{{{ as-destroy-buffer-delete-file

(defun as-destroy-buffer-delete-file ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (save-buffer)
  (let ((fn (buffer-file-name)))
    (delete-file fn)
    (kill-buffer nil)
    (message (format "Deleted %s" fn))))

;;}}}

;;{{{ bury-and-close-buffer

(defun bury-and-close-buffer ()
  (interactive)
  (bury-buffer) 
  (when (not (one-window-p))
    (delete-window)))

;;}}}
;;{{{ mhj-set-q-to-close

(defun mhj-set-q-to-close ()
  (local-set-key "q" 'bury-and-close-buffer))

;;}}}

