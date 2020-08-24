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
    (load-theme 'sunlight)))

(require 'as-toggles)
(use-package hydra
  :config
  (defhydra+ hydra-toggle (:color pink)
    ("o" as-toggle-sunlight-theme "toggle sunlight theme")))

;; These are not needed as long as the themes directory is in
;; custom-theme-load-path:
;;
;; (use-feature pastels-on-dark-aspiers-theme
;;   :no-require t)
;; 
;; (use-feature sunlight-theme
;;   :no-require t)

(provide 'as-color-theme)
