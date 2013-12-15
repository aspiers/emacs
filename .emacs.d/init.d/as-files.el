;; File handling

(use-package tar-mode
  :mode ("\\.dump$" . tar-mode))

(use-package auto-compression-mode
  :defer t
  :config
  (defun lac () "Load auto-compression-mode."
    (interactive)
    (auto-compression-mode 1)))

(use-package recentf
  :if window-system
  :init
  (recentf-mode t))

(global-set-key "\C-c+"   'make-directory)
(global-set-key "\C-cR"   'as-rename-current-buffer-file)
(global-set-key "\C-ck"   'delete-file)
(global-set-key "\C-cK"   'as-destroy-buffer-delete-file)
(global-set-key [(control x) (meta f)]    'find-library)

(autoload 'find-file-at-point "ffap" nil t)
(autoload 'ffap-other-window  "ffap" nil t)
(autoload 'ffap-other-frame   "ffap" nil t)
(global-set-key [(control meta ?')]       'find-file-at-point)
(global-set-key [(control x) ?4 ?']       'ffap-other-window)
(global-set-key [(control x) ?5 ?']       'ffap-other-frame)

(defun as-find-from-home ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/" "Find file: "))
(global-set-key [(control ~)] 'as-find-from-home)
(global-set-key "\C-cjh"      'as-find-from-home)
