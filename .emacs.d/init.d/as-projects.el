(custom-set-variables '(projectile-keymap-prefix "p"))

(require 'as-key-chord)
(require 'as-magit)

(use-package projectile
  :defer 5
  :after as-key-chord

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
  (setq projectile-switch-project-action 'as-projectile-switch-project))

(with-packages (projectile as-editing)
  (require 'as-editing) ;; for as-copy-previous-line-suffix
  :bind ("C-c p" . as-copy-previous-line-suffix))

(defun as-projectile-switch-project (&optional project-root)
  "Adam's wrapper around `projectile-vc' which just switches to
an existing magit status buffer if it exists, to save rebuilding it."
  (interactive (and current-prefix-arg
                    (list
                     (projectile-completing-read
                      "Open project VC in: "
                      projectile-known-projects))))
  (or project-root (setq project-root (projectile-project-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (if (eq vcs 'git)
        (magit-status-quick)
      (projectile-vc project-root))))

(defun as-counsel-projectile-switch-project (project)
  "Open PROJECT in vc-dir / magit / monky."
  (let ((projectile-switch-project-action 'as-projectile-switch-project))
    (counsel-projectile-switch-project-by-name project)))

;; https://github.com/raxod502/selectrum/issues/281
;; How to achieve similar with selectrum?  Maybe this?
;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#search-with-ripgrep-like-counsel-rg
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (define-key projectile-mode-map [remap projectile-grep] 'counsel-projectile-grep))

(with-packages (projectile selectrum)
  :config
  (setq projectile-completion-system 'default))

(use-package helm-projectile
  :defer 10)

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
