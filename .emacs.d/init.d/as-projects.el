;; Used to install via el-get:
;;
;; https://github.com/dimitri/el-get/issues/1471
;; https://github.com/dimitri/el-get/issues/1472
;; https://github.com/dimitri/el-get/issues/1473
;; Finally managed to install by fixing dependencies manually
;; (#1471), #1472, #1473

(custom-set-variables '(projectile-keymap-prefix "p"))

(req-package projectile
  :config
  ;; https://github.com/bbatsov/projectile/issues/287
  (defvar projectile-prefix-map
    (lookup-key projectile-mode-map (kbd "C-c p"))
    "The keymap which Projectile typically binds to C-c p.")
  (bind-key "C-x p" projectile-prefix-map)
  (global-set-key (kbd "C-c p") 'previous-line)
  (local-set-key (kbd "C-c p") 'previous-line)
  (global-set-key [(control ?c) ?p] 'previous-line)
  (define-key mode-specific-map (kbd "p") nil)
  (bind-key "C-c p" 'as-copy-previous-line-suffix)
  (require 'as-key-chord)

  ;; Not needed since guide-key/recursive-key-sequence-flag is set:
  ;; (setq guide-key/guide-key-sequence
  ;;       '("<key-chord> z p" "<key-chord> p z"))
  (key-chord-define-global "zp" projectile-prefix-map)

  (key-chord-define-global "ZG" 'projectile-grep)
  (key-chord-define-global "zm" 'projectile-commander)
  (key-chord-define-global "zs" 'projectile-switch-project)
  (key-chord-define-global "zf" 'projectile-find-file)
  (key-chord-define-global "zb" 'projectile-switch-to-buffer)

  ;; https://github.com/bbatsov/projectile/issues/496
  (projectile-mode))

(require 'as-find-file-in-dir)
(define-find-file-in-dir-function as-find-my-mrconfig
  "~/.config/mr/" "Find mr config: ")
(bind-key "C-c j m"  'as-find-my-mrconfig)

(define-find-file-in-dir-function as-find-stow-package
  "~/.STOW/" "Find stow package: ")
(bind-key "C-c j s"  'as-find-stow-package)

(provide 'as-projects)
