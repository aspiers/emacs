(use-package rg
  :chords ("zr" . rg)
  :chords ("ZR" . rg-dwim)
  :config (rg-enable-default-bindings))

;; These are used by projectile:
(use-package ripgrep
  :after projectile)
(use-package ag
  :after projectile)

(provide 'as-search)
