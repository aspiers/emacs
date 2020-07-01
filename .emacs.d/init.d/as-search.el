(use-package rg
  :chords ("zr" . rg)
  :config (rg-enable-default-bindings))

;; These are used by projectile:
(use-package ripgrep
  :after projectile)
(use-package ag
  :after projectile)

(provide 'as-search)
