(req-package scilla-mode
  :require flycheck
  :config
  (setq scilla-root (concat (getenv "HOME") "/blockchain/scilla/scilla")))

(provide 'as-scilla)
