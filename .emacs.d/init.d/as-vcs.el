;; Version control

(req-package vc-osc
  :config
  (add-to-list 'vc-handled-backends 'osc 'append))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t))

(use-package find-file-in-dir
 :ensure nil
  :config
  (define-find-file-in-dir-function as-find-CVS-repo
    "~/.CVS/" "Find CVS repo: ")
  (define-find-file-in-dir-function as-find-my-git-repo
    "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: ")
  (define-find-file-in-dir-function as-find-upstream-git-repo
    "~/.GIT/3rd-party/" "Find 3rd-party git repo: ")

  :bind (("C-c j C" . as-find-CVS-repo)
         ("C-c j g" . as-find-my-git-repo)
         ("C-c j 3" . as-find-upstream-git-repo)
         ("C-c j G" . as-find-upstream-git-repo)))

(use-package git-timemachine
  :commands git-timemachine)

(require 'as-magit)

(req-package gerrit-download)

(provide 'as-vcs)
