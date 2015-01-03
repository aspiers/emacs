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

(bind-key "C-c +"   'make-directory)
(bind-key "C-c R"   'as-rename-current-buffer-file)
(bind-key "C-c k"   'delete-file)
(bind-key "C-c K"   'as-destroy-buffer-delete-file)
(bind-key "C-x M-f" 'find-library)

(use-package ffap
  :bind
  (("C-M-'"   . find-file-at-point)
   ("C-x 4 '" . ffap-other-window)
   ("C-x 5 '" . ffap-other-frame)))

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-from-home "~/")
(bind-key "C-~"     'as-find-from-home)
(bind-key "C-c j h" 'as-find-from-home)

(define-find-file-in-dir-function as-find-from-root "/sudo:root@localhost:/")
(bind-key "C-c j /" 'as-find-from-root)

(use-package as-find-file-matching-regexp-hook)
