(req-package zenburn-theme
  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t))

(provide 'as-color-theme)
