;; Configuration languages

(req-package rpm-spec-mode
  :mode ("\\.spec$" . rpm-spec-mode))

(req-package apache-mode
  :mode ("\\.htaccess$\\|\\(httpd\\|srm\\|access\\)\\.conf$" . apache-mode))

(req-package xrdb-mode
  :mode ("\\.Xdefaults$"    . xrdb-mode)
  :mode ("\\.Xenvironment$" . xrdb-mode)
  :mode ("\\.Xresources$"   . xrdb-mode)
  :mode (".*\\.ad$"         . xrdb-mode)
  :mode (".*\\.x?rdb$"      . xrdb-mode)
  :config
  (add-hook 'xrdb-mode-hook (lambda () (setq comment-start "! "))))

(provide 'as-config-langs)
