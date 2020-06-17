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
(defun as-next-svn-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*svn-status*")))
(bind-key "C-c s b"   'as-next-svn-buffer)
;; (require 'psvn)

;;}}}
;;{{{ vc-osc

(req-package vc-osc
  :config
  (add-to-list 'vc-handled-backends 'osc 'append))

;;}}}
;;{{{ git-gutter

(req-package git-gutter+
  :config
  (global-git-gutter+-mode t))

(req-package git-gutter-fringe+)

;;}}}

(use-package as-find-file-in-dir
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

(require 'as-magit)

(req-package gerrit-download)

(provide 'as-vcs)
