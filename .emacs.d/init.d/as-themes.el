;; (use-package color-theme-modern)

(use-package zenburn-theme
  :no-require t

  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000"))))

(add-to-list 'custom-theme-load-path as-themes-dir)

(use-package modus-themes
  :straight (:type git :host github :repo "protesilaos/modus-themes")
  :ensure t)

;; Since ef-themes 2.0, ef-themes is built on top of modus-themes.
;; All ef-themes commands (select, toggle, etc.) delegate to
;; modus-themes equivalents, and ef-themes user options are aliases
;; for the corresponding modus-themes options.
(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :requires modus-themes
  :ensure t
  :init
  ;; This makes the Modus commands listed below consider only the Ef
  ;; themes.  For an alternative that includes Modus and all
  ;; derivative themes (like Ef), enable the
  ;; `modus-themes-include-derivatives-mode' instead.  The manual of
  ;; the Ef themes has a section that explains all the possibilities:
  ;;
  ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
  ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
  (ef-themes-take-over-modus-themes-mode 1)

  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))

  :custom
  ;; Note: ef-themes-to-toggle is an alias for modus-themes-to-toggle,
  ;; but :custom setting doesn't propagate correctly, so we set it in :config below.
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)

  :config
  ;; All customisations here.
  ;; (setq modus-themes-mixed-fonts t)
  ;; (setq modus-themes-italic-constructs t)

  ;; Set toggle pair explicitly since the alias doesn't work via :custom:
  (setq modus-themes-to-toggle '(ef-summer ef-winter))

  ;; Use slightly off-black for dark themes to avoid terminal color mapping
  ;; issues (#000000 gets mapped to white in some terminals).
  (setq modus-themes-preset-overrides-dark
        '((bg-main . "#050505")
          (bg-main-intense . "#1a1a1a")))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; https://protesilaos.com/modus-themes/#h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")
  (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
  (set-face-attribute 'default nil :family "Fira Code")

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-winter))

(defun as-set-theme (target-theme)
  "Set theme to 'light' or 'dark'.
Uses `modus-themes-to-toggle' to determine the light/dark pair."
  (let* ((light (car modus-themes-to-toggle))
         (dark (cadr modus-themes-to-toggle)))
    (modus-themes-select
     (if (string= target-theme "light") light dark))))

(defun as-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (let* ((pair modus-themes-to-toggle)
         (light (car pair))
         (dark (cadr pair))
         (enabled custom-enabled-themes)
         (current (car enabled)))
    (mapc #'disable-theme enabled)
    (modus-themes-select (if (and current (memq light (list current))) dark light))))

(require 'as-toggles)
(use-package hydra
  :config
  (defhydra+ hydra-toggle (:color blue)
    ("t" as-toggle-theme "toggle theme")))

;; These are not needed as long as the themes directory is in
;; custom-theme-load-path:
;;
;; (use-feature pastels-on-dark-aspiers-theme
;;   :no-require t)
;;
;; (use-feature sunlight-theme
;;   :no-require t)

(provide 'as-themes)
