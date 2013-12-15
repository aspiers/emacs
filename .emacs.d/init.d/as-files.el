;; File handling

(use-package tar-mode
  :mode ("\\.dump$" . tar-mode))

(use-package auto-compression-mode
  :defer t
  :config
  (defun lac () "Load auto-compression-mode."
    (interactive)
    (auto-compression-mode 1)))

(use-package recentf
  :if window-system
  :init
  (recentf-mode t))
