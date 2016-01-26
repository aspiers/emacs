(req-package phi-search
  :bind (("C-c m C-s" . phi-search)
         ("C-c m C-r" . phi-search-backward))
  :config
  ;; FIXME: change to req-package
  (require 'phi-search-mc)
  (define-key phi-search-default-map (kbd "C-g") 'phi-search-abort))

(provide 'as-phi-search)
