;; Configuration languages
;;
;; See also as-tech-langs.el and as-doc-langs.el

(req-package rpm-spec-mode
  :mode ("\\.spec$" . rpm-spec-mode))

(req-package apache-mode
  :mode ("\\.htaccess$\\|\\(httpd\\|srm\\|access\\)\\.conf$" . apache-mode))

(req-package xrdb-mode
  ;; Can't get this working:
  ;; https://github.com/edvorg/req-package/issues/29
  :loader (el-get)
  :mode ("\\.Xdefaults$"    . xrdb-mode)
  ;; :mode ("\\.Xenvironment$" . xrdb-mode)
  ;; :mode ("\\.Xresources$"   . xrdb-mode)
  ;; :mode (".*\\.ad$"         . xrdb-mode)
  ;; :mode (".*\\.x?rdb$"      . xrdb-mode)
  :config
  (add-hook 'xrdb-mode-hook (lambda () (setq comment-start "! "))))

(provide 'as-config-langs)
