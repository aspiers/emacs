(req-package zenburn-theme
  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t))

(add-to-list 'custom-theme-load-path as-themes-dir)

;; These are not needed as long as the themes directory is in
;; custom-theme-load-path:
;;
;; (req-package pastels-on-dark-aspiers-theme
;;   :no-require t
;;   :ensure nil)
;; 
;; (req-package sunlight-theme
;;   :no-require t
;;   :ensure nil)

(provide 'as-color-theme)
