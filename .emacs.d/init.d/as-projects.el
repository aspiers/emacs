;; Used to install via el-get:
;;
;; https://github.com/dimitri/el-get/issues/1471
;; https://github.com/dimitri/el-get/issues/1472
;; https://github.com/dimitri/el-get/issues/1473
;; Finally managed to install by fixing dependencies manually
;; (#1471), #1472, #1473

(custom-set-variables '(projectile-keymap-prefix "p"))

(require 'as-key-chord)
(use-package projectile
  :requires as-key-chord

  ;; Not needed since guide-key/recursive-key-sequence-flag is set:
  ;; (setq guide-key/guide-key-sequence
  ;;       '("<key-chord> z p" "<key-chord> p z"))
  :chords (("zp" . projectile-prefix-map)
           ("ZG" . projectile-grep)
           ("zm" . projectile-commander)
           ("zs" . projectile-switch-project)
           ("zf" . projectile-find-file)
           ("zd" . projectile-find-dir)
           ("zb" . projectile-switch-to-buffer))

  :bind
  (:map projectile-command-map
        ("DEL" . projectile-remove-known-project))

  :config
  ;; https://github.com/bbatsov/projectile/issues/287
  (defvar projectile-prefix-map
    (lookup-key projectile-mode-map (kbd "C-c p"))
    "The keymap which Projectile typically binds to C-c p.")
  (bind-key "C-x p" projectile-prefix-map)
  (define-key mode-specific-map (kbd "p") nil)

  ;; https://github.com/bbatsov/projectile/issues/496
  (projectile-mode)

  :bind ("C-c p" . as-copy-previous-line-suffix))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(require 'find-file-in-dir)

(define-find-file-in-dir-function as-find-my-mrconfig
  "~/.config/mr/groups.d" "Find mr config: ")
(define-find-file-in-dir-function as-find-stow-package
  "~/.STOW/" "Find stow package: ")

;; See as-jump.el / as-package-loading.el for explanation of usage
(use-feature as-jump
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("m" "mrconfig" . as-find-my-mrconfig)
             ("S" "~/.STOW/" . as-find-stow-package)))

;; Other interesting project-related packages:
;;
;;      projectile-codesearch
;;      projectile-variable

(provide 'as-projects)
