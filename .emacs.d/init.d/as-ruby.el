(req-package ruby-mode
  :mode "\\(\\.\\(e?rb\\|rjs\\|rake\\)\\|Rakefile\\|Guardfile\\)\\'"
  :interpreter "ruby\\|ruby[12]\.[0-9]\\|jruby\\|rbx")

(add-to-list 'auto-mode-alist '("\\.gem\\'" . tar-mode))

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

(req-package autotest :commands autotest)

;; I don't use this and it seems to have bugs with autoloading
;; and maybe also leaving processes running within emacs.
;; (req-package inf-ruby)

(req-package rsense
  :config
  (progn
   (defvar rsense-home (concat (getenv "HOME") "/.STOW/rsense"))
   (add-to-list 'load-path (concat rsense-home "/etc"))))

(require 'as-jump)
(use-package bundler
  ;; Need to load which-key to ensure define-key is advised
  :after which-key
  :config
  (bind-keys :map as-jump-map
             ("o" "Ruby bundler gems" . bundle-open)))

(req-package rubocop)
;;(req-package rudel)

(provide 'as-ruby)
