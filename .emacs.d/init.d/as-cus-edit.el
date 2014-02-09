(require 'as-el-get)
(require 'as-versions)

(when (version<= emacs-version "24.3.1")
  (require 'cus-edit)
  (defun custom-save-variables ()
    "Save all customized variables in `custom-file'."
    (save-excursion
      (custom-save-delete 'custom-set-variables)
      (let ((standard-output (current-buffer))
            (saved-list (make-list 1 0))
            sort-fold-case)
        ;; First create a sorted list of saved variables.
        (mapatoms
         (lambda (symbol)
           (if (and (get symbol 'saved-value)
                    ;; ignore theme values
                    (or (null (get symbol 'theme-value))
                        (eq 'user (caar (get symbol 'theme-value)))))
               (nconc saved-list (list symbol)))))
        (setq saved-list (sort (cdr saved-list) 'string<))
        (unless (bolp)
          (princ "\n"))
        (princ "(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.\n")
        (dolist (symbol saved-list)
          (let ((spec (car-safe (get symbol 'theme-value)))
                (value (get symbol 'saved-value))
                (requests (get symbol 'custom-requests))
                (now (and (not (custom-variable-p symbol))
                          (or (boundp symbol)
                              (eq (get symbol 'force-value)
                                  'rogue))))
                (comment (get symbol 'saved-variable-comment)))
            ;; Check REQUESTS for validity.
            (dolist (request requests)
              (when (and (symbolp request) (not (featurep request)))
                (message "Unknown requested feature: %s" request)
                (setq requests (delq request requests))))
            ;; Is there anything customized about this variable?
            (when (or (and spec (eq (car spec) 'user))
                      comment
                      (and (null spec) (get symbol 'saved-value)))
              ;; Output an element for this variable.
              ;; It has the form (SYMBOL VALUE-FORM NOW REQUESTS COMMENT).
              ;; SYMBOL is the variable name.
              ;; VALUE-FORM is an expression to return the customized value.
              ;; NOW if non-nil means always set the variable immediately
              ;; when the customizations are reloaded.  This is used
              ;; for rogue variables
              ;; REQUESTS is a list of packages to load before setting the
              ;; variable.  Each element of it will be passed to `require'.
              ;; COMMENT is whatever comment the user has specified
              ;; with the customize facility.
              (unless (bolp)
                (princ "\n"))
              (princ " '(")
              (prin1 symbol)
              (princ " ")
              (let ((val (prin1-to-string (car value))))
                (if (< (length val) 60)
                    (insert val)
                  (newline-and-indent)
                  (let ((beginning-of-val (point)))
                    (insert val)
                    (save-excursion
                      (goto-char beginning-of-val)
                      (indent-pp-sexp 1)))))
              (when (or now requests comment)
                (princ " ")
                (prin1 now)
                (when (or requests comment)
                  (princ " ")
                  (prin1 requests)
                  (when comment
                    (princ " ")
                    (prin1 comment))))
              (princ ")"))))
        (if (bolp)
            (princ " "))
        (princ ")")
        (unless (looking-at-p "\n")
          (princ "\n"))))))

(provide 'as-cus-edit)
