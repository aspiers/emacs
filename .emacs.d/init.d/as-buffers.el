;; Anything to do with buffer management and switching 

;; Set C-x C-b to buffer-menu rather than list-buffers so that the
;; point automatically gets put in the buffer menu.
(bind-key "C-x C-b" 'buffer-menu)

;; (use-package bs
;;   :bind ("C-x C-b" . bs-show))

(defun as-revert-buffer-without-confirmation ()
  (interactive)
  (revert-buffer t t))

(require 'as-mode-lighters)

(use-feature autorevert :diminish 'auto-revert-mode)

(use-package frog-jump-buffer
  :bind ("C-!" . frog-jump-buffer))

(use-feature as-destroy-buffer
  :bind ("C-x K" . as-destroy-buffer))
(bind-key "C-c r" 'as-revert-buffer-without-confirmation)
(bind-key "C-c b" 'bury-buffer)
(bind-key "C-;"   'switch-to-prev-buffer)
(bind-key "C-:"   'switch-to-next-buffer)
(bind-key "C-c B" 'as-bounce-buffer)
(bind-key "C-x I" 'insert-buffer)
(bind-key "<f6>"  'as-bounce-buffer)
(bind-key "C-c C-S-r" 'as-rename-current-buffer)

(use-feature as-display-buffer-names
  :bind (("C-c n" . as-display-buffer-filename)
         ("C-c N" . as-display-buffer-name)))

(use-package switch-window
  :bind ("C-x o" . switch-window))

(use-package minimap
  :commands minimap-mode)

;; (use-package sublimity
;;   :config
;;   (sublimity-mode 1)
;;   (require 'sublimity-map)
;;   (sublimity-map-set-delay 0.1)
;;   (sublimity-map-show))

(provide 'as-buffers)
