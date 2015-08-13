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

    ;; Improved and updated version of this nice idea from
    ;; http://iqbalansari.github.io/blog/2014/02/22/switching-repositories-with-magit/
    (eval-after-load 'projectile
      (progn
        (require 'tramp)
        (setq magit-repository-directories
              (mapcar (lambda (dir) (directory-file-name dir))
                      ;; remove tramp and non-git projects
                      (remove-if
                       (lambda (project)
                         (or
                          (tramp-tramp-file-p project)
                          (not (file-directory-p (concat project "/.git")))))
                       (projectile-relevant-known-projects))))
        (setq magit-repo-dirs-depth 1)))))

(use-package magit-topgit)

(provide 'as-magit)
