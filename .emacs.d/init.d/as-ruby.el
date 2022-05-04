(use-package ruby-mode
  :mode "\\(\\.\\(e?rb\\|rjs\\|rake\\)\\|Rakefile\\|Guardfile\\)\\'"
  :interpreter "ruby\\|ruby[12]\\.[0-9]\\|jruby\\|rbx")

(use-package flymake-ruby)

(use-feature tar-mode
  :mode ("\\.gem\\'" . tar-mode))

;;{{{ rcov.el

;; corrections for rcov.el, although it kinda looks lame - no red/green overlaying

(autoload 'rcov "rcov" "rcov" t)

(defvar rcov-command-line "rake test:units:rcov RCOVOPTS='--gcc --no-html'"
  "Rcov command line to find uncovered code.
It is good to use rcov with Rake because it `cd's appropriate directory.
`--gcc' option is strongly recommended because `rcov' uses compilation-mode.")
(defvar rcovsave-command-line "rake rcov RCOVOPTS='--gcc --no-html --save=coverage.info'"
  "Rcov command line to save coverage status. See also `rcov-command-line'.")
(defvar rcovdiff-command-line "rake rcov RCOVOPTS='-D --gcc --no-html'"
  "Rcov command line to find new uncovered code. See also `rcov-command-line'.")

;;}}}

(autoload 'rcov-buffer "rcov-overlay" "rcov-overlay" t)
;; (bind-key "C-c C-r"   'rcov-buffer)

(use-package autotest :commands autotest)

;; I don't use this and it seems to have bugs with autoloading
;; and maybe also leaving processes running within emacs.
;; (use-package inf-ruby)

(require 'as-jump)
(use-package bundler
  ;; Need to load which-key to ensure define-key is advised
  :after which-key
  :config
  ;; (defvar as-jump-ruby-map (make-sparse-keymap "Jump to Ruby")
  ;;   "Adam's prefix keymap for quickly jumping to Ruby stuff")
  ;; (bind-keys :map as-jump-ruby-map
  ;;            ("g" "Ruby bundler gems" . bundle-open))

  ;; Can't name prefix keymap - reported in:
  ;; https://github.com/justbur/emacs-which-key/issues/253
  ;; (define-key as-jump-map "r" '("Ruby" . as-jump-ruby-map))
  ;; (bind-keys :map as-jump-map ("r" "Ruby" . as-jump-ruby-map))
  (bind-keys :map as-jump-map
             :prefix "r"
             :prefix-map as-jump-ruby-map
             :prefix-docstring "foo"
             :menu-name "ruby map"
             ("g" "Ruby bundler gems" . bundle-open))

  ;; Poor workaround which doesn't work for key chords:
  ;; (which-key-add-key-based-replacements "C-c j r" "Ruby")

  (push '((nil . "as-jump-ruby-map") . (nil . "Ruby")) which-key-replacement-alist))


(use-package rubocop)
;; (use-package rudel)

(provide 'as-ruby)
