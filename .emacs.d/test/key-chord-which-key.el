(package-initialize)

(package-install 'bundler)

(package-install 'key-chord)
(key-chord-mode 1)

(setq which-key-enable-extended-define-key t)
(package-install 'which-key)
(which-key-mode)

(defvar as-jump-map (make-sparse-keymap "Jump to stuff"))
(global-set-key (kbd "C-c j") as-jump-map)
(key-chord-define-global "zj" as-jump-map)
(key-chord-define-global "zr" 'dired)

(defvar as-jump-ruby-map (make-sparse-keymap "Jump to Ruby")
  "Adam's prefix keymap for quickly jumping to Ruby stuff")
(define-key as-jump-map "r" as-jump-ruby-map)
(define-key as-jump-ruby-map "g" 'bundle-open)
