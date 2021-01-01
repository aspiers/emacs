(use-package server
  :commands (server-start server-force-delete)
  :defer 10
  :config
  (defun server-restart (interactive)
    "Equivalent to `server-force-delete' followed by `server-start'."
    (server-force-delete)
    (server-start))
  (unless (as-quick-startup)
    (condition-case err
        (server-start)
      (file-error (message "%s" (error-message-string err))))))

(use-package edit-server
  :commands (edit-server-start edit-server-stop)
  :defer 10
  :config
  (defun edit-server-restart (interactive)
    "Equivalent to `edit-server-stop' followed by `edit-server-start'."
    (edit-server-stop)
    (edit-server-start))
  (unless (as-quick-startup)
    (condition-case err
        (edit-server-start)
      (file-error (message "%s" (error-message-string err))))))

(use-package edit-server-htmlize
  :commands (edit-server-maybe-dehtmlize-buffer
             edit-server-maybe-htmlize-buffer)
  :after edit-server
  :hook
  ((edit-server-start . edit-server-maybe-dehtmlize-buffer)
   (edit-server-done .  edit-server-maybe-htmlize-buffer)))

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

(with-packages (atomic-chrome markdown-mode)
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame))

(provide 'as-server)
