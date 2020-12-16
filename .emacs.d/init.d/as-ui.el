;; see also as-smart-mode-line.el

(use-package fill-column-indicator
  :straight (fill-column-indicator
             :fork (:repo "aspiers/Fill-Column-Indicator"
                          :branch "wrap-window-edge"))
  :config
  (dolist (hook '(prog-mode-hook indented-text-mode))
    (add-hook hook 'fci-mode)))

(bind-key "C-'" 'speedbar-get-focus)

(use-package beacon
  :defer 5
  :config (beacon-mode 1))

(use-package flycheck)
(use-package hideshow-org)

(use-package insert-char-preview
    :commands insert-char-preview
    :bind ("C-x 8 RET" . insert-char-preview))

;; Not impressed with this.  No native projectile integration, and can't
;; only display open files.
;;
;; (use-package neotree
;;   :bind ("C-'" . neotree-toggle)
;;   :after (all-the-icons projectile)
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-smart-open t)
;;   ;; But I want this to be 'projectile-vc!
;;   (setq projectile-switch-project-action 'neotree-projectile-action))

(provide 'as-ui)
