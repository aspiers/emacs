(use-package yasnippet
  :commands yas-minor-mode
  :init
  (defalias 'yasm 'yas-minor-mode)
  :config
  (progn
    (global-set-key [(control <)] 'yasnippet-cmd-previous-real)
    (global-set-key [(control >)] 'yasnippet-cmd-next-real)))

(provide 'as-yasnippet)
