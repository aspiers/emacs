;; Version control

;;{{{ cvs helper modes

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
