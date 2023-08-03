(use-package org-noter)

(use-package org-notion
  :straight (:host github :repo "richardwesthaver/org-notion")
  :custom
  (org-notion-keymap-prefix "C-c M-n")
  :hook (org-mode . org-notion-mode))

(provide 'as-org-integrations)
