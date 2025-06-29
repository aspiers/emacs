;; This seems to fix invisible UTF org-bullets
(use-package unicode-fonts
  :if (<= emacs-major-version 27)
  :config (unicode-fonts-setup))

;; See also https://emacsredux.com/blog/2023/03/16/setting-the-default-font-for-emacs/
;; (set-face-attribute 'default nil :family "Hack")
(set-face-attribute 'default nil :family "Hack Nerd Font")
;; (set-face-attribute 'default nil :family "Maple Mono NF")
;; (set-face-attribute 'default nil :family "SauceCodePro Nerd Font")

(set-face-attribute 'default nil :height 150)

;; This seems to do the same
;; (set-frame-font "14" nil t)
;; (set-frame-font "Source Code Pro 14" nil t)

(provide 'as-fonts)
