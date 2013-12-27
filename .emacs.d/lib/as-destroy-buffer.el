;;{{{ as-destroy-buffer

;;;###autoload
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

;;;###autoload
(defun as-destroy-buffer-delete-file ()
  "Kill the current buffer and delete the associated file."
  (interactive)
  (save-buffer)
  (let ((fn (buffer-file-name)))
    (delete-file fn)
    (kill-buffer nil)
    (message (format "Deleted %s" fn))))

;;}}}

(provide 'as-destroy-buffer)
