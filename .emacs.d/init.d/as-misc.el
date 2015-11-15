(require 'as-vars)

;;{{{ find-function-source-path

;; Don't need this, as the Makefile now copys all .el files into the
;; installdir alongside the .elc files, and `find-library' looks in
;; `load-path'.

;; (defvar as-emacs-dir)

;; (custom-set-variables
;;  '(find-function-source-path
;;    (append load-path
;;       (mapcar (lambda (p) (concat as-emacs-dir "/" p))
;;               (list
;;                "fun"
;; [snipped]
;;                "utils"
;;                )))))

;;}}}
;;{{{ e-mail address

(setq user-mail-address "adam@spiers.net")

;;}}}
;;{{{ kill-line kill whole line if at beginning of line

(setq kill-whole-line t)

;;}}}
;;{{{ Visible bell

(setq-default visible-bell t)

;;}}}
;;{{{ Saving sessions

(autoload 'desktop-save "desktop" "Saves desktop session state." t)

;;}}}
;;{{{ midnight-mode - automatically kill unused buffers

(use-package midnight
  :if (not (as-quick-startup)))

;;}}}
;;{{{ client/server mode

(use-package server
  :commands (server-start server-force-delete)
  :init
  (progn
    (defun ss ()
      "Abbreviation for `server-start'."
      (interactive)
      (server-start))
    (defun server-restart (interactive)
      "Equivalent to `server-force-delete' followed by `server-start'."
      (server-force-delete)
      (server-start))
    (unless (as-quick-startup)
      (condition-case err
          (server-start)
        (file-error (message "%s" (error-message-string err)))))))

(use-package edit-server
  :commands (edit-server-start edit-server-stop)
  :defer 10
  :config
  (unless (as-quick-startup)
    (condition-case err
        (edit-server-start)
      (file-error (message "%s" (error-message-string err))))
    (defun edit-server-restart (interactive)
      "Equivalent to `edit-server-stop' followed by `edit-server-start'."
      (edit-server-stop)
      (edit-server-start))))

(use-package edit-server-htmlize
  :commands (edit-server-maybe-dehtmlize-buffer
             edit-server-maybe-htmlize-buffer)
  :init
  ;; FIXME: use req-package
  (when (featurep 'edit-server)
    (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
    (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)))

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
;;{{{ Color themes

(use-package color-theme)
(use-package color-theme-pastels-on-dark
  ;; FIXME: use req-package
  :if (featurep 'color-theme)
  :config
  (color-theme-pastels-on-dark))

;;}}}

(provide 'as-misc)
