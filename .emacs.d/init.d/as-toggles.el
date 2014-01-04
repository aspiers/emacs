;; _T_oggles and settings (C-c t)

;;{{{ as-toggle-indent-tabs-mode

(defun as-toggle-indent-tabs-mode ()
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

;;}}}
(bind-key "C-c t b"   'as-toggle-indent-tabs-mode)

;;{{{ as-toggle-case-fold-search

(defun as-toggle-case-fold-search ()
  "Toggles the value of case-fold-search in the current buffer"
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "case-fold-search set to %s" case-fold-search))

;;}}}
(bind-key "C-c t c"   'as-toggle-case-fold-search)

;;{{{ as-toggle-debug-on-error

(defun as-toggle-debug-on-error ()
  "Toggles the value of debug-on-error in the current buffer"
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

;;}}}
(bind-key "C-c t e"   'as-toggle-debug-on-error)

(bind-key "C-c t f"   'auto-fill-mode)

;;{{{ as-toggle-truncate-lines

(defun as-toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display)
  (message "truncate-lines set to %s" truncate-lines))

;;}}}
(bind-key "C-c t s"   'as-toggle-truncate-lines) ;; mnemonic: less -S

;;{{{ as-set-tab-width

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

;;}}}
(bind-key "C-c t w"   'as-set-tab-width)

(provide 'as-toggles)
