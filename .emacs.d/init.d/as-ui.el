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

(provide 'as-ui)
