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

(require 'git-gutter)
(global-git-gutter-mode t)

;;}}}
;;{{{ C-c g for git operations

(bind-key "C-c g b"  'magit-run-git-gui-blame)
(bind-key "C-c g g"  'magit-run-git-gui)
(bind-key "C-c g k"  'magit-run-gitk)
(bind-key "C-c g s"  'magit-status)
(bind-key "C-S-g"    'magit-status)

(autoload 'ido-buffer-internal "ido")
(defvar ido-default-buffer-method)
(defun ido-switch-magit-buffer ()
  "Switch to a magit status buffer via `ido'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method
                       nil "magit status: " nil "*magit: "))
(bind-key "C-M-g" 'ido-switch-magit-buffer)

;;}}}

(defun as-find-CVS-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.CVS/" "Find CVS repo: "))
(bind-key "C-c j c"  'as-find-CVS-repo)

(defun as-find-my-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: "))
(bind-key "C-c j g"  'as-find-my-git-repo)

(defun as-find-upstream-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/3rd-party/" "Find 3rd-party git repo: "))
(bind-key "C-c j 3"  'as-find-upstream-git-repo)
(bind-key "C-c j G"  'as-find-upstream-git-repo)

