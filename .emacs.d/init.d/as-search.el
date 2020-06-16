(use-package rg
  :chords ("zr" . rg)
  :config (rg-enable-default-bindings))

;; These are used by projectile:
(use-package ripgrep)
(use-package ag)

(provide 'as-search)
