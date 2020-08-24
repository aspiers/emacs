(bind-key "C-c G" 'customize-group)
(bind-key "C-c A" 'customize-apropos)

(defun as-expand-sexp ()
  "Expand a sexp so that each word is on a separate line."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)
                        (save-excursion
                          (forward-sexp)
                          (point)))
      (while (re-search-forward "\\([^[:space:]\n]\\) +\\([^[:space:]\n]\\)" nil t)
        (replace-match "\\1\n\\2")
        (forward-char -1))))
  (indent-pp-sexp))

(defun as-expand-psp ()
  "In `custom-file', expand package-selected-packages so that
each package is on a separate line in order to minimise merge
conflicts."
  (with-current-buffer
      (find-buffer-visiting custom-file)
    (save-excursion
      (goto-char (point-min))
      (cond ((re-search-forward "'(package-selected-packages" nil 'noerror)
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
             ;; (warn "package-selected-packages not found in %s" custom-file)
             )))))

(defun as-custom-save-hook ()
  "Hook to tweak Adam's custom-file before saving it."
  (when (eq (current-buffer) (find-buffer-visiting custom-file))
    (as-expand-psp)))

(add-hook 'before-save-hook 'as-custom-save-hook)

(provide 'as-custom-edit)
