(use-package smart-mode-line
  :config
  (progn
    (sml/setup)
    ;; https://github.com/Bruce-Connor/smart-mode-line/issues/51
    (when (version<= emacs-version "24.3")
      (unless (member mode-line-front-space mode-line-format)
        (setq-default mode-line-format (cons mode-line-front-space mode-line-format))))))

;; Note: use rich-minority to hide minor modes

(provide 'as-smart-mode-line)
