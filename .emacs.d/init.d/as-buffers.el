;; Anything to do with buffer management and switching 

;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.
(bind-key "C-x C-b" 'buffer-menu)

(req-package bs
  :bind ("C-x C-b" . bs-show))

(defun as-revert-buffer-without-confirmation ()
  (interactive)
  (revert-buffer t t))

(bind-key "C-x K" 'as-destroy-buffer)
(bind-key "C-!"   'ido-switch-buffer)
(bind-key "C-="   'switch-to-prev-buffer)
(bind-key "C-+"   'switch-to-next-buffer)
(bind-key "C-c r" 'as-revert-buffer-without-confirmation)
(bind-key "C-c b" 'bury-buffer)
(bind-key "C-;"   'bury-buffer)
(bind-key "C-c B" 'as-bounce-buffer)
(bind-key "C-x I" 'insert-buffer)
(bind-key "<F6>"  'as-bounce-buffer)
(bind-key "C-c n" 'as-display-buffer-filename)
(bind-key "C-c N" 'as-display-buffer-name)
(bind-key "C-c C-S-r" 'as-rename-current-buffer)

(req-package switch-window
  :bind ("C-x o" . switch-window))

(provide 'as-buffers)
