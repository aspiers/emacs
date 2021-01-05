(with-packages (projectile)
  :config
  (defun as-edit-git-assembly ()
    "Edit .git/assembly file within current git project."
    (interactive)
    (if (projectile-project-p)
        (if (eq (projectile-project-vcs) 'git)
            (find-file (projectile-expand-root ".git/assembly"))
          (error "Not in a git project"))
      (error "Not in a project")))

  :bind ("C-c g y" . as-edit-git-assembly))

(provide 'as-git)
