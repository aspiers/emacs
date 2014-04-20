(use-package phi-search
  :bind (("C-s" . phi-search)
         ("C-r" . phi-search-backward))
  :init (require 'phi-search-mc))

(provide 'as-phi-search)
