;; Version control

(use-feature vc-osc
  :straight (:local-repo "~/.GIT/adamspiers.org/vc-osc")
  :config
  (add-to-list 'vc-handled-backends 'osc 'append))

(require 'find-file-in-dir)

(define-find-file-in-dir-function as-find-CVS-repo
  "~/.CVS/" "Find CVS repo: ")
(define-find-file-in-dir-function as-find-my-git-repo
  "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: ")
(define-find-file-in-dir-function as-find-upstream-git-repo
  "~/.GIT/3rd-party/" "Find 3rd-party git repo: ")

;; See as-jump.el / as-package-loading.el for explanation of usage
(use-feature as-jump
  :after which-key

  :config
  (bind-keys :map as-jump-map
             ("C" "CVS repos" . as-find-CVS-repo)
             ("g" "my git repos" . as-find-my-git-repo)
             ("3" "3rd party git repos" . as-find-upstream-git-repo)))

(use-package git-timemachine
  :commands git-timemachine)

(require 'as-magit)

(use-package gerrit-download)

(provide 'as-vcs)
