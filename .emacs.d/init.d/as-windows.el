(global-set-key [(control \,)]            'delete-other-windows)
(global-set-key [(control .)]             'delete-window)
(global-set-key [(control tab)]           'other-window)
(defun previous-window-interactive ()
  "Interactive wrapper around (other-window -1) for key binding purposes."
  (interactive)
  (other-window -1))
(global-set-key [(control shift tab)]     'previous-window-interactive)
(global-set-key [C-S-iso-lefttab]         'previous-window-interactive)

(provide 'as-windows)
