(bind-key "C-,"               'delete-other-windows)
(bind-key "C-."               'delete-window)
(bind-key "C-<tab>"           'other-window)
(defun previous-window-interactive ()
  "Interactive wrapper around (other-window -1) for key binding purposes."
  (interactive)
  (other-window -1))
(bind-key "C-S-<tab>"         'previous-window-interactive)
(bind-key "C-S-<iso-lefttab>" 'previous-window-interactive)

;; There's also ace-window but it doesn't have fancy large numbers
(use-package switch-window
  :bind (("C-X o" . switch-window)
         ("C-X 1" . switch-window-then-maximize)
         ("C-X 2" . switch-window-then-split-below)
         ("C-X 3" . switch-window-then-split-right)
         ("C-X 0" . switch-window-then-delete)

         ("C-X 4 d" . switch-window-then-dired)
         ("C-X 4 f" . switch-window-then-find-file)
         ("C-X 4 m" . switch-window-then-compose-mail)
         ("C-X 4 r" . switch-window-then-find-file-read-only)

         ("C-X 4 C-f" . switch-window-then-find-file)
         ("C-X 4 C-o" . switch-window-then-display-buffer)

         ("C-X 4 0" . switch-window-then-kill-buffer))
  :config
  (setq switch-window-multiple-frames t))

;; TODO: check out https://www.emacswiki.org/emacs/WinnerMode

;; (use-package golden-ratio)

(provide 'as-windows)
