(defvar projectile-prefix-map
  (lookup-key projectile-mode-map projectile-keymap-prefix)
  "The keymap which Projectile typically binds to C-c p.")
(global-set-key "\C-xp" projectile-prefix-map)

;; Not needed since guide-key/recursive-key-sequence-flag is set:
;; (setq guide-key/guide-key-sequence
;;       '("<key-chord> z p" "<key-chord> p z"))
(key-chord-define-global "zp" projectile-prefix-map)

(key-chord-define-global "zm" 'projectile-commander)
(key-chord-define-global "pf" 'projectile-find-file)
(key-chord-define-global "pb" 'projectile-switch-to-buffer)

(defun as-find-my-mrconfig ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.config/mr/" "Find mr config: "))
(global-set-key "\C-cjm"  'as-find-my-mrconfig)

(defun as-find-stow-package ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.STOW/" "Find stow package: "))
(global-set-key "\C-cjs"  'as-find-stow-package)

(provide 'as-projects)
