;; see also as-smart-mode-line.el, as-selection.el

;; Native replacement for fci-mode from emacs 27.1
(dolist (hook '(prog-mode-hook indented-text-mode))
  (add-hook hook 'display-fill-column-indicator--turn-on))

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
