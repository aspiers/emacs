(load-library "as-loaddefs")
(require 'as-require)

(as-progress "functions...")

;;{{{ Appearance

;;{{{ as-font-lock-mode-if-window-system

(defun as-font-lock-mode-if-window-system
  ()
  "Turns on font-lock mode if X windows is active."
  (interactive)
  (if window-system (font-lock-mode)))

;;}}}

;;}}}
;;{{{ elisp helper functions

(autoload 'function-arg-types "as-elisp")
(autoload 'function-arity "as-elisp")

;;}}}
;;{{{ ll => load-library

(defun ll (library)
  "Shortcut to `load-library'."
  (interactive "sLoad library: ")
  (load-library library))

;;}}}
;;{{{ Enable disabled functions

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left     'disabled nil)

;;}}}

(as-progress "functions...done")

