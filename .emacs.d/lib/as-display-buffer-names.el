;;{{{ as-display-buffer-filename

;; (eval-when-compile
;;   (autoload 'x-select-text "term/x-win" nil t))

;;;###autoload
(defun as-display-buffer-filename (&optional mode)
  "Displays the current buffer's filename in the minibuffer.

If a prefix argument is given, stores the result in the kill ring
and in the X selection for other programs.

If the prefix argument is 2, forks the external program abs (must
be in $PATH) to convert the filename to an absolute one with all
symlinks resolved.

If the prefix argument is 3 or 16, calculates the path relative to the
project root."
  (interactive "p")
  (let ((file-name
         (cond ((eq major-mode 'Info-mode)
                Info-current-file)
               ((eq major-mode 'dired-mode)
                dired-directory)
               (buffer-file-name)
               (t
                (error "No file associated with this buffer")))))
    (let ((fn (cond ((eq mode 2)
                     (substring
                      (shell-command-to-string (concat "abs " file-name))
                      0 -1))
                    ((or (= mode 3) (= mode 16))
                     (file-relative-name file-name (projectile-project-root)))
                    (t file-name))))
      (cond (mode
             (kill-new fn)
             ;; No need for x-select-text since kill-new calls function set
             ;; in interprogram-cut-function, which is x-select-text anyway.
             ;;         (x-select-text buffer-file-name)
             (setq fn (concat fn " (stored in kill ring and X selection)"))))
      (message "%s" fn))))

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
    (message "%s" fn)))

;;}}}

(provide 'as-display-buffer-names)
