(req-package zenburn-theme
  :no-require t

  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000"))))

;; To choose a specific theme, either do something like this here:
;;
;;    (load-theme 'zenburn t)
;;
;; or customize the custom-enabled-themes variable.

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
