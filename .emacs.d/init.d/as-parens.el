(require 'paren)

(req-package smartparens)

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(provide 'as-parens)
