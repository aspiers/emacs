(req-package server
  :commands (server-start server-force-delete)
  :defer 10
  :config
  (defun ss ()
    "Abbreviation for `server-start'."
    (interactive)
    (server-start))
  (defun server-restart (interactive)
    "Equivalent to `server-force-delete' followed by `server-start'."
    (server-force-delete)
    (server-start))
  (unless (as-quick-startup)
    (condition-case err
        (server-start)
      (file-error (message "%s" (error-message-string err))))))

(req-package edit-server
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

(req-package edit-server-htmlize
  :commands (edit-server-maybe-dehtmlize-buffer
             edit-server-maybe-htmlize-buffer)
  ;; FIXME: use req-package
  :after 'edit-server
  :config
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer))

(provide 'as-server)
