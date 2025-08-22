(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun my-consult-buffer-tramp-setup ()
  "Conditionally disable ivy-rich columns for `consult-buffer` when on TRAMP."
  (when (file-remote-p default-directory)
    ;; Disable all columns for `consult-buffer`
    (ivy-rich-del-all-column 'consult-buffer)))

;; Add a hook that runs before `consult-buffer` starts.
(add-hook 'consult-buffer-hook 'my-consult-buffer-tramp-setup)

(provide 'as-tramp)
