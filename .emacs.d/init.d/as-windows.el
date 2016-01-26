(bind-key "C-,"               'delete-other-windows)
(bind-key "C-."               'delete-window)
(bind-key "C-<tab>"           'other-window)
(defun previous-window-interactive ()
  "Interactive wrapper around (other-window -1) for key binding purposes."
  (interactive)
  (other-window -1))
(bind-key "C-S-<tab>"         'previous-window-interactive)
(bind-key "C-S-<iso-lefttab>" 'previous-window-interactive)

(req-package switch-window)

;; (req-package golden-ratio)

(provide 'as-windows)
