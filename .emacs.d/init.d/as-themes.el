;; (use-package color-theme-modern)

(use-package zenburn-theme
  :no-require t

  :config
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000"))))

(add-to-list 'custom-theme-load-path as-themes-dir)

(use-package ef-themes
  :custom
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).

  ;; If you like two specific themes and want to switch between them, you
  ;; can specify them in `ef-themes-to-toggle' and then invoke the command
  ;; `ef-themes-toggle'.  All the themes are included in the variable
  ;; `ef-themes-collection'.
  (ef-themes-to-toggle '(ef-summer ef-winter))

  ;; (ef-themes-headings ; read the manual's entry or the doc string
  ;;       '((0 variable-pitch light 1.9)
  ;;         (1 variable-pitch light 1.8)
  ;;         (2 variable-pitch regular 1.7)
  ;;         (3 variable-pitch regular 1.6)
  ;;         (4 variable-pitch regular 1.5)
  ;;         (5 variable-pitch 1.4)      ; absence of weight means `bold'
  ;;         (6 variable-pitch 1.3)
  ;;         (7 variable-pitch 1.2)
  ;;         (t variable-pitch 1.1)))

  ;; They are nil by default...
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)

  ;; Read the doc string or manual for this one.  The symbols can be
  ;; combined in any order.
  (ef-themes-region '(intense no-extend neutral))

  :config
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  ;; (load-theme 'ef-summer :no-confirm)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  (ef-themes-select 'ef-winter))

;; The themes we provide are recorded in the `ef-themes-dark-themes',
;; `ef-themes-light-themes'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-select-dark'
;; - `ef-themes-select-light'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

(use-package modus-themes
  :config

  (modus-themes-load-themes)
  ;; https://protesilaos.com/modus-themes/#h:ea30ff0e-3bb6-4801-baf1-d49169d94cd5
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")
  (set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
  (set-face-attribute 'default nil :family "Fira Code")

  ;; Either load-theme here, or customize the custom-enabled-themes variable.
  ;; (modus-themes-load-vivendi)
  )

(defun as-terminal-profile (profile)
  "Call xfce4-terminal-config and gnome-terminal-profile to set the
active terminal profiles."
  (start-process "xfce4-terminal-config" nil
                 "xfce4-terminal-config" profile)
  (start-process "gnome-terminal-profile" nil
                 "gnome-terminal-profile" profile)
  (with-temp-file
      (expand-file-name "~/.config/gnome-terminal-profile")
    (insert profile "\n")))

(defun as-toggle-bright-theme ()
  "Toggles a bright theme for outdoors vs. indoors."
  (interactive)
  (cond ((or
          (memq 'ef-summer custom-enabled-themes)
          (memq 'ef-winter custom-enabled-themes))
         (let ((new-theme (ef-themes-toggle)))
           (as-terminal-profile
            (pcase new-theme
              ('ef-summer "Bright")
              ('ef-winter "Dark")))))
        (t
         ;; toggle modus-theme
         (let ((dark-theme 'modus-vivendi)
               (bright-theme 'modus-operandi))
           ;; or sunlight and pastels-on-dark-aspiers
           (if (memq bright-theme custom-enabled-themes)
               (progn
                 (disable-theme bright-theme)
                 (load-theme dark-theme)
                 (as-terminal-profile "Dark"))
             (disable-theme dark-theme)
             (load-theme bright-theme)
             (as-terminal-profile "Bright"))))))

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
