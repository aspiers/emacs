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
  :bind (("C-S-x o" . switch-window)
         ("C-S-x 1" . switch-window-then-maximize)
         ("C-S-x 2" . switch-window-then-split-below)
         ("C-S-x 3" . switch-window-then-split-right)
         ("C-S-x 0" . switch-window-then-delete)

         ("C-S-x 4 d" . switch-window-then-dired)
         ("C-S-x 4 f" . switch-window-then-find-file)
         ("C-S-x 4 m" . switch-window-then-compose-mail)
         ("C-S-x 4 r" . switch-window-then-find-file-read-only)

         ("C-S-x 4 C-f" . switch-window-then-find-file)
         ("C-S-x 4 C-o" . switch-window-then-display-buffer)

         ("C-S-x 4 0" . switch-window-then-kill-buffer))

  :custom
  (switch-window-multiple-frames t))

;; TODO: check out https://www.emacswiki.org/emacs/WinnerMode

;; (use-package golden-ratio)

(provide 'as-windows)
