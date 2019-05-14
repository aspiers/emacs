(setq scilla-root (concat (getenv "HOME") "/blockchain/scilla/scilla"))

(req-package scilla-mode
  :require flycheck
  :load-path (lambda () (concat scilla-root "/misc/emacs-mode")))

(provide 'as-scilla)
