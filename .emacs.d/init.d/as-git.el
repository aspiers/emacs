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

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))

(use-package hydra
  :config
  ;; https://github.com/abo-abo/hydra/wiki/Version-Control
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _n_: next hunk        _s_: st^a^ge hunk     _q_uit
  _p_: previous hunk    _v_: re_v_ert hunk    _Q_uit and deactivate git-gutter
  ^ ^                 _SPC_: po^p^up hunk
  _f_: first hunk
  _l_: last hunk        _R_: set start revision
"
    ("n" git-gutter:next-hunk)
    ("p" git-gutter:previous-hunk)
    ("f" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("v" git-gutter:revert-hunk)
    ("SPC" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue))

  :bind ("M-G M-g" . hydra-git-gutter/body))


(provide 'as-git)
