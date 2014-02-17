(require 'as-el-get)

(use-package smart-mode-line
  :config
  (progn
    (sml/setup)
    (unless (member mode-line-front-space mode-line-format)
      (setq-default mode-line-format (cons mode-line-front-space mode-line-format)))))

(provide 'as-smart-mode-line)
