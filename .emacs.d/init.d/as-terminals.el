(use-package vterm)

(use-package vterm-toggle
  :bind (("C-`" . vterm-toggle)
         ("M-`" . vterm-toggle-forward)
         ("M-~" . vterm-toggle-backward)))

(provide 'as-terminals)
