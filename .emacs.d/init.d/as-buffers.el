;; Anything to do with buffer management and switching 

;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.
(global-set-key [(control x)(control b)] 'buffer-menu)

;; But if bs-show is available, choose that cos it's much nicer.
(if (functionp 'bs-show)
     (global-set-key [(control x)(control b)]
                     (lambda (arg)
                       (interactive "P")
                       (bs-show arg)
                       (let ((inhibit-read-only t))
                         (save-excursion
                           (goto-char (point-max))
                           (insert "\n"))))))

(global-set-key [(control x) K] 'as-destroy-buffer)
(global-set-key [(control !)]             'ido-switch-buffer)
(global-set-key [(control =)]             'switch-to-prev-buffer)
(global-set-key [(control +)]             'switch-to-next-buffer)
(global-set-key "\C-cr"   'revert-buffer)
(global-set-key "\C-cb"   'bury-buffer)
(global-set-key [(control \;)]            'bury-buffer)
(global-set-key "\C-cB"   'as-bounce-buffer)
(global-set-key [(control x) (I)]         'insert-buffer)
(global-set-key [(f6)] 'as-bounce-buffer)
(global-set-key "\C-cn"   'as-display-buffer-filename)

(provide 'as-buffers)
