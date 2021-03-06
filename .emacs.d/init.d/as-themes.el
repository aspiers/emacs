;; (use-package color-theme-modern)

(use-package zenburn-theme
  :no-require t

  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000"))))

(use-package modus-operandi-theme)

(use-package modus-vivendi-theme
  :config

  ;; https://protesilaos.com/modus-themes/#h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 110)
  (set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 110)
  (set-face-attribute 'default nil :family "Fira Code" :height 105)

  ;; Either load-theme here, or customize the custom-enabled-themes variable.
  (load-theme 'modus-vivendi t))

(add-to-list 'custom-theme-load-path as-themes-dir)

(defun as-toggle-bright-theme ()
  "Toggles a bright theme for outdoors vs. indoors."
  (interactive)
  (let ((dark-theme 'modus-vivendi)
        (bright-theme 'modus-operandi))
    ;; or sunlight and pastels-on-dark-aspiers
    (if (memq bright-theme custom-enabled-themes)
        (progn
          (disable-theme bright-theme)
          (load-theme dark-theme))
      (disable-theme dark-theme)
      (load-theme bright-theme))))

(require 'as-toggles)
(use-package hydra
  :config
  (defhydra+ hydra-toggle (:color blue)
    ("t" as-toggle-bright-theme "toggle bright theme")))

;; These are not needed as long as the themes directory is in
;; custom-theme-load-path:
;;
;; (use-feature pastels-on-dark-aspiers-theme
;;   :no-require t)
;; 
;; (use-feature sunlight-theme
;;   :no-require t)

(provide 'as-themes)
