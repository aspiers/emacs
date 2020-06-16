;; _T_oggles and settings (C-c t)

;;{{{ as-toggle-indent-tabs-mode

(defun as-toggle-indent-tabs-mode ()
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

;;}}}

(bind-key "C-c t b"   'as-toggle-indent-tabs-mode)
(bind-key "C-c t c"   'toggle-case-fold-search)
(bind-key "C-c t e"   'toggle-debug-on-error)
(bind-key "C-c t f"   'auto-fill-mode)
(bind-key "C-c t s"   'toggle-truncate-lines) ;; mnemonic: less -S

;;{{{ as-set-tab-width

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

;;}}}
(bind-key "C-c t w"   'as-set-tab-width)

(provide 'as-toggles)
