(require 'paren)

(req-package smartparens)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'as-parens)
