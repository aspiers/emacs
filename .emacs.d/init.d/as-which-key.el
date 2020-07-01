(use-package which-key
  :defer 10
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  (defun as-which-key-delay-function (prefix length)
    (cond ((string-match-p
            "C-c j\\|<key-chord> z j"
            prefix)
           0)
          (t 1.0)))
  (add-to-list 'which-key-delay-functions 'as-which-key-delay-function))

(setq which-key-enable-extended-define-key t)

(provide 'as-which-key)
