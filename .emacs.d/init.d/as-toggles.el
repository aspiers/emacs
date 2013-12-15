;; _T_oggles and settings (C-c t)

;;{{{ as-toggle-indent-tabs-mode

(defun as-toggle-indent-tabs-mode ()
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

;;}}}
(global-set-key "\C-ctb"   'as-toggle-indent-tabs-mode)

;;{{{ as-toggle-case-fold-search

(defun as-toggle-case-fold-search ()
  "Toggles the value of case-fold-search in the current buffer"
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "case-fold-search set to %s" case-fold-search))

;;}}}
(global-set-key "\C-ctc"   'as-toggle-case-fold-search)

;;{{{ as-toggle-debug-on-error

(defun as-toggle-debug-on-error ()
  "Toggles the value of debug-on-error in the current buffer"
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error set to %s" debug-on-error))

;;}}}
(global-set-key "\C-cte"   'as-toggle-debug-on-error)

(global-set-key "\C-ctf"   'auto-fill-mode)

;;{{{ as-toggle-truncate-lines

(defun as-toggle-truncate-lines
  ()
  "Toggles the value of truncate lines in the current buffer"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (redraw-display)
  (message "truncate-lines set to %s" truncate-lines))

;;}}}
(global-set-key "\C-cts"   'as-toggle-truncate-lines) ;; mnemonic: less -S

;;{{{ as-set-tab-width

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

;;}}}
(global-set-key "\C-ctw"   'as-set-tab-width)

(provide 'as-toggles)
