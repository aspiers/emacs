;; see also as-smart-mode-line.el, as-selection.el

;; Native replacement for fci-mode from emacs 27.1
(use-feature display-fill-column-indicator
  :config
  (set-face-attribute 'fill-column-indicator nil :weight 'thin)
  (dolist (hook '(prog-mode-hook indented-text-mode))
    (add-hook hook (lambda () (display-fill-column-indicator-mode +1)))))

(use-package beacon
  :defer 5
  :config (beacon-mode 1))

(use-package flycheck)
(use-package hideshow-org)

(use-package insert-char-preview
    :commands insert-char-preview
    :bind ("C-x 8 RET" . insert-char-preview))

(use-package prism
  :straight (:host github :repo "alphapapa/prism.el")
  ;; :hook ((emacs-lisp-mode . prism-mode)
  ;;        (python-mode . prism-mode))
  )

;; (use-package transient-posframe
;;   :straight (:host github :repo "yanghaoxie/transient-posframe")
;;   :config
;;   (transient-posframe-mode))

(provide 'as-ui)
