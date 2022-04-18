;; This seems to fix invisible UTF org-bullets
(use-package unicode-fonts
  :if (<= emacs-major-version 27)
  :config (unicode-fonts-setup))

(provide 'as-fonts)
