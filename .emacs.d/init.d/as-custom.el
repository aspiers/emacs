;; Need to load this before org-mode packages:
;;
;;   * org-disputed-keys and org-replace-disputed-keys won't work
;;     unless set before org.el is loaded for the first time

(require 'as-vars)
(require 'as-make-backup-file-name nil 'noerror) ;; need as-make-backup-file-name

(setq custom-file (format "%s/as-custom-%s.el"
                          as-init-dir emacs-version-number))

(as-progress "loading %s ..." custom-file)
;; This load is required, according to the info pages:
(load custom-file)
(as-progress "loaded %s" custom-file)

(unless (fboundp 'as-make-backup-file-name)
  (if (eq 'as-make-backup-file-name make-backup-file-name-function)
      (setq make-backup-file-name-function nil)))

(defun as-expand-sexp ()
  "Expand a sexp so that each word is on a separate line."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (forward-sexp)
                          (point)))
      (while (re-search-forward " +\b" nil t)
        (replace-match "\n"))))
  (indent-pp-sexp))

(defun as-expand-psp ()
  "In `custom-file', expand package-selected-packages so that
each package is on a separate line in order to minimise merge
conflicts."
  (with-current-buffer
      (find-buffer-visiting custom-file)
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward "'(package-selected-packages")
             (or (re-search-forward "(quote")
                 (error
                  "Couldn't find \"(quote\" after package-selected-packages in %s"
                  custom-file))
             (or (re-search-forward "(")
                 (error
                  "Couldn't find starting paren after package-selected-packages in %s"
                  custom-file))
             (forward-char -1)
             (as-expand-sexp))
            (t
             (warn "package-selected-packages not found in %s"
                   custom-file))))))

(defun as-custom-save-hook ()
  "Hook to tweak Adam's custom-file before saving it."
  (when (eq (current-buffer) (find-buffer-visiting custom-file))
    (as-expand-psp)))

(add-hook 'before-save-hook 'as-custom-save-hook)

(provide 'as-custom)
