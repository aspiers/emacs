;; one of these two will work
(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(defalias 'perl-mode 'cperl-mode)

(add-hook 'perl-mode-hook 'as-font-lock-mode-if-window-system)

(autoload 'as-cperl-setup "as-cperl" "as-cperl-setup")
(add-hook 'cperl-mode-hook 'as-cperl-setup)
(add-hook 'cperl-mode-hook 'turn-on-auto-fill)
(defalias 'cp 'cperl-mode "Abbreviation for `cperl-mode'.")
(add-hook 'cperl-mode-hook (lambda () (setq comment-start "#")))

(add-to-list 'auto-mode-alist '("\\.\\(pod\\|t\\)\\'" . cperl-mode))

(provide 'as-perl)
