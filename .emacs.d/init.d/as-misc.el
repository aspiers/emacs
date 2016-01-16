(require 'as-vars)

;;{{{ Saving sessions

(autoload 'desktop-save "desktop" "Saves desktop session state." t)

;;}}}
;;{{{ midnight-mode - automatically kill unused buffers

(req-package midnight
  :if (not (as-quick-startup)))

;;}}}
;;{{{ Diary, appointments

;; suspect I don't need this any more
(autoload 'appt-make-list "appt")
(add-hook 'diary-hook 'appt-make-list)

;;}}}
;;{{{ UTF support

(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

(define-coding-system-alias 'UTF-8 'utf-8)

;;}}}

(provide 'as-misc)
