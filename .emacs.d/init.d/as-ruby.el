(use-package ruby-mode
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

(use-package autotest :commands autotest)

(use-package rsense
  :config
  (progn
   (defvar rsense-home (concat (getenv "HOME") "/.STOW/rsense"))
   (add-to-list 'load-path (concat rsense-home "/etc"))))

(autoload 'bundle-open "bundler" nil t)
(bind-key "C-c j b" 'bundle-open)

(provide 'as-ruby)
