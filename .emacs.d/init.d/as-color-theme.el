(req-package zenburn-theme
  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t))

(req-package sunlight-theme
  :no-require t
  :ensure nil)

(provide 'as-color-theme)
