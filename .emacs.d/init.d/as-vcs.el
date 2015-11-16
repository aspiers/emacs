;; Version control

;;{{{ cvs helper modes

;;;###autoload
(defun bury-and-close-buffer ()
  (interactive)
  (bury-buffer)
  (when (not (one-window-p))
    (delete-window)))

;;;###autoload
(defun mhj-set-q-to-close ()
  (local-set-key "q" 'bury-and-close-buffer))

;; diff mode
(add-hook 'diff-mode-hook 'mhj-set-q-to-close)

;; cvs-status mode
(add-hook 'cvs-status-mode-hook 'mhj-set-q-to-close)

;; log-view mode
(add-hook 'log-view-mode-hook 'mhj-set-q-to-close)

;;}}}
;;{{{ psvn

(autoload 'svn-status "psvn" "svn-status" t)
(bind-key "C-c s s" 'svn-status)
(bind-key "C-c s u" 'svn-status-update-cmd)
(fset 'as-next-svn-buffer "C-xb*svn-status*")
(bind-key "C-c s b"   'as-next-svn-buffer)
;; (require 'psvn)

;;}}}
;;{{{ vc-osc

(use-package vc-osc
  :config
  (add-to-list 'vc-handled-backends 'osc 'append))

;;}}}
;;{{{ git-gutter

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;;}}}

;; For magit, see as-magit.el

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-CVS-repo
  "~/.CVS/" "Find CVS repo: ")
(bind-key "C-c j C"  'as-find-CVS-repo)

(define-find-file-in-dir-function as-find-my-git-repo
  "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: ")
(bind-key "C-c j g"  'as-find-my-git-repo)

(define-find-file-in-dir-function as-find-upstream-git-repo
  "~/.GIT/3rd-party/" "Find 3rd-party git repo: ")
(bind-key "C-c j 3"  'as-find-upstream-git-repo)
(bind-key "C-c j G"  'as-find-upstream-git-repo)

(provide 'as-vcs)
