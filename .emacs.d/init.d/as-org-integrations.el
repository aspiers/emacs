(use-package org-noter)

(use-package org-notion
  :straight (:host github :repo "richardwesthaver/org-notion")
  :hook (org-mode . org-notion-mode))
