;; This seems to fix invisible UTF org-bullets
(use-package unicode-fonts
  :if (<= emacs-major-version 27)
  :config (unicode-fonts-setup))

(set-face-attribute 'default nil :height 140)
;; This seems to do the same
;; (set-frame-font "14" nil t)
;; (set-frame-font "Source Code Pro 14" nil t)

(provide 'as-fonts)
