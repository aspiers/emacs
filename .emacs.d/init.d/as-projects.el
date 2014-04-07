(defvar projectile-prefix-map
  (lookup-key projectile-mode-map (kbd "C-c p"))
  "The keymap which Projectile typically binds to C-c p.")
(bind-key "C-x p" projectile-prefix-map)

(require 'as-key-chord)

;; Not needed since guide-key/recursive-key-sequence-flag is set:
;; (setq guide-key/guide-key-sequence
;;       '("<key-chord> z p" "<key-chord> p z"))
(key-chord-define-global "zp" projectile-prefix-map)

(key-chord-define-global "zm" 'projectile-commander)
(key-chord-define-global "zs" 'projectile-switch-project)
(key-chord-define-global "zf" 'projectile-find-file)
(key-chord-define-global "zb" 'projectile-switch-to-buffer)

(defun as-find-my-mrconfig ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.config/mr/" "Find mr config: "))
(bind-key "C-c j m"  'as-find-my-mrconfig)

(defun as-find-stow-package ()
  (interactive)
  (ido-file-internal ido-default-file-method
                     nil "~/.STOW/" "Find stow package: "))
(bind-key "C-c j s"  'as-find-stow-package)

(provide 'as-projects)
