;;{{{ as-display-buffer-filename

;; (eval-when-compile
;;   (autoload 'x-select-text "term/x-win" nil t))

;;;###autoload
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
;;{{{ as-display-buffer-name

;;;###autoload
(defun as-display-buffer-name (&optional prefix)
  "Displays the current buffer's name in the minibuffer.

If a prefix argument is given, stores the result in the kill ring
and in the X selection for other programs."
  (interactive "P")
  (let ((fn (buffer-name)))
    (cond (prefix
           (kill-new fn)
           (setq fn (concat fn " (stored in kill ring and X selection)"))))
    (message fn)))

;;}}}

(provide 'as-display-buffer-names)
