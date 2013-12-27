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
(global-set-key "\C-css" 'svn-status)
(global-set-key "\C-csu" 'svn-status-update-cmd)
(fset 'as-next-svn-buffer "\C-xb*svn-status*")
(global-set-key "\C-csb"   'as-next-svn-buffer)
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

(global-set-key "\C-cgb"  'magit-run-git-gui-blame)
(global-set-key "\C-cgg"  'magit-run-git-gui)
(global-set-key "\C-cgk"  'magit-run-gitk)
(global-set-key "\C-cgs"  'magit-status)
(global-set-key [(control shift g)] 'magit-status)

(autoload 'ido-buffer-internal "ido")
(defvar ido-default-buffer-method)
(defun ido-switch-magit-buffer ()
  "Switch to a magit status buffer via `ido'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method
                       nil "magit status: " nil "*magit: "))
(global-set-key [(control meta g)] 'ido-switch-magit-buffer)

;;}}}

(defun as-find-CVS-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.CVS/" "Find CVS repo: "))
(global-set-key "\C-cjc"  'as-find-CVS-repo)

(defun as-find-my-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/adamspiers.org/" "Find adamspiers.org git repo: "))
(global-set-key "\C-cjg"  'as-find-my-git-repo)

(defun as-find-upstream-git-repo ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.GIT/3rd-party/" "Find 3rd-party git repo: "))
(global-set-key "\C-cj3"  'as-find-upstream-git-repo)
(global-set-key "\C-cjG"  'as-find-upstream-git-repo)

