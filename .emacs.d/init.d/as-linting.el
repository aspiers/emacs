(use-package prettier
  :config
  (dir-locals-set-class-variables 'prettier
                                  '((nil . ((eval . (prettier-mode t)))))))

(provide 'as-linting)
