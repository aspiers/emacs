(use-package magit
  :config
  (progn

    (add-hook 'ido-setup-hook 'as-magit-ido-keys)

    (defun as-magit-ido-keys ()
      "Add Adam's keybindings for ido."
      (define-key ido-file-dir-completion-map (kbd "C-Y") 'ido-magit-status-internal)
      (define-key ido-file-dir-completion-map (kbd "S-<return>") 'ido-magit-status-internal))

    (defun ido-magit-status-internal ()
      "Run magit-status on current directory.  For use with
`ido-file-dir-completion-map'."
      (interactive)
      (progn
        ;; Based on part of `ido-magic-backward-char' which gets invoked
        ;; when C-d is hit during ido mode (see ido-common-completion-map).
        (setq fallback 'magit-status)
        (setq ido-exit 'fallback)
        (exit-minibuffer)))

    (defun ido-magit-status ()
      "Call `magit-status' the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, see `ido-find-file'."
      (interactive)
      (magit-status
       (ido-read-directory-name "magit status: " nil nil t)))))
