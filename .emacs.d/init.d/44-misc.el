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

(unless (as-quick-startup) (as-soft-require 'midnight))

;;}}}
;;{{{ client/server mode

(autoload 'server-start "server" "server-start" t)
(defun ss ()
  "Abbreviation for `server-start'."
  (interactive)
  (server-start))

(autoload 'edit-server-start "edit-server" "edit-server" t)
(autoload 'edit-server-stop  "edit-server" "edit-server" t)
(unless (as-quick-startup)
  (server-start)
  (condition-case err
      (edit-server-start)
    (file-error (message "%s" (error-message-string err))))
  (defun edit-server-restart (interactive)
    "Equivalent to `edit-server-stop' followed by `edit-server-start'."
    (edit-server-stop)
    (edit-server-start)))

(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;;}}}
;;{{{ Diary, appointments

;; suspect I don't need this any more
(autoload 'appt-make-list "appt")
(add-hook 'diary-hook 'appt-make-list)

;;}}}
;;{{{ UTF-16 support

(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE.*\x0D\x00$" . utf-16-le-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF.*\x0D\x00$" . utf-16-be-dos) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFF\xFE" . utf-16-le) t)
(add-to-list 'auto-coding-regexp-alist '("^\xFE\xFF" . utf-16-be) t)

(defun utf-16-le-pre-write-conversion (start end) nil)
(defun utf-16-be-pre-write-conversion (start end) nil)

;;}}}
;;{{{ Color themes

(require 'color-theme-autoloads nil 'noerror)
(autoload 'color-theme-pastels-on-dark "color-theme-pastels-on-dark" "pastels-on-dark-theme" t)
(if (as-check-feature-loaded 'color-theme-autoloads)
    (progn
      (require 'pastels-on-dark-theme nil 'noerror)
      (if (as-check-feature-loaded 'pastels-on-dark-theme)
          (color-theme-pastels-on-dark)
        (message "Couldn't load pastels-on-dark-theme")))
  (message "Couldn't load color-theme"))

;;}}}
