;; File handling

(defun dir-locals-show-current-buffer ()
  "Show the directory local variables configured for the current
buffer. These are not necessarily the same as the active values."
  (interactive)
  (message "%s" (dir-locals-find-file (buffer-file-name))))

(defun dired-current-file ()
  "Open dired and jump the point to the current buffer's file."
  (interactive)
  (if (not (buffer-file-name))
      (message "Current buffer has no file")
    (let ((current-file (file-name-nondirectory (buffer-file-name))))
      (dired ".")
      (beginning-of-buffer)
      (re-search-forward
       (concat " " (regexp-quote current-file) "\\( \\|$\\)"))
      (goto-char (match-beginning 0)))))

(with-packages key-chord
  :config
  (key-chord-define-global "xd" 'dired-current-file)
  (key-chord-define-global "ZL" 'dir-locals-show-current-buffer))

(use-package tar-mode
  :mode ("\\.dump$" . tar-mode))

(use-feature auto-compression-mode
  :defer t
  :config
  (defun lac () "Load auto-compression-mode."
    (interactive)
    (auto-compression-mode 1)))

(use-package recentf
  :if window-system
  :config
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

(require 'find-file-in-dir)

(define-find-file-in-dir-function as-find-from-home "~/")
(define-find-file-in-dir-function as-find-from-root
  "/sudo:root@localhost:/")

(bind-keys ("C-~"  . as-find-from-home)
           ("C-\"" . as-find-personal-todo))

;; See as-jump.el / as-package-loading.el for explanation of usage
(use-feature as-jump
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("h" "Find from ~" . as-find-from-home)
             ("/" "Find from /" . as-find-from-root)))

(use-feature as-find-file-matching-regexp-hook)

(require 'as-vars)
;; This is right for SUSE, at least, assuming that the
;; emacs-debugsource package is installed.
(setq find-function-C-source-directory
      (format "/usr/src/debug/emacs-%s/src" emacs-version-number))

(provide 'as-files)
