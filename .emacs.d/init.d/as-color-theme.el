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

(defun as-toggle-sunlight-theme ()
  "Toggles the sunlight theme for outdoors vs. indoors."
  (interactive)
  (if (memq 'sunlight custom-enabled-themes)
      (disable-theme 'sunlight)
    (enable-theme 'sunlight)))

(bind-key "C-c t o" 'as-toggle-sunlight-theme)

;; These are not needed as long as the themes directory is in
;; custom-theme-load-path:
;;
;; (use-package pastels-on-dark-aspiers-theme
;;   :no-require t
;;   :ensure nil)
;; 
;; (use-package sunlight-theme
;;   :no-require t
;;   :ensure nil)

(provide 'as-color-theme)
